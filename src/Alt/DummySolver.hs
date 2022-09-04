
module Alt.DummySolver where

import Control.Monad
import Control.Monad.State
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HMS
-- import qualified Data.Set as S
import Debug.Trace
import Text.Printf

import Codec.Picture.Types

import Types
import PNG
import Alt.AST
import Alt.Interpreter
import Json

data SolverState = SolverState {
      ssProgram :: Program
    , ssMergedBlocks :: M.Map BlockId Color
    , ssAvgColorsByBlock :: M.Map BlockId Color
    , ssConfiguration :: Configuration
  }

initSolverState :: Configuration -> SolverState
initSolverState cfg = SolverState [] M.empty M.empty cfg

type SolverM a = StateT SolverState (State InterpreterState) a

runSolver :: SolverM () -> SolverState -> InterpreterState -> Program
runSolver solver st ist =
  let interpret = execStateT solver st
      st' = evalState interpret ist
  in  reverse $ ssProgram st'

issueMove :: Move -> SolverM ()
issueMove m = do
  lift $ interpretMove m
  modify $ \st -> st {ssProgram = m : ssProgram st}

paintWithAvgColors :: FilePath -> FilePath -> IO Program
paintWithAvgColors cfgPath imgPath = do
  img <- readPngImage imgPath
  cfg <- parseConfig cfgPath
  let blocksCfg = getColoredBlocks cfg
      paint (blockId, shape, existingColor) =
        let avgColor = calcAvgColor img shape
        in  if avgColor == existingColor
              then Nothing
              else Just $ SetColor blockId avgColor
  return $ mapMaybe paint blocksCfg

getBlocksByPos :: SolverM (M.Map Point (BlockId, Shape))
getBlocksByPos = do
  blocks <- lift $ gets isBlocks
  return $ M.fromList [(Point (rX block) (rY block), (blockId, block)) | (blockId, block) <- HMS.toList blocks]

lookupBlockByPos :: Point -> SolverM (Maybe (BlockId, Shape))
lookupBlockByPos point = do
  blocksByPos <- getBlocksByPos
  return $ M.lookup point blocksByPos

findBlockAtRight :: Shape -> SolverM (Maybe (BlockId, Shape))
findBlockAtRight shape = do
  let nextPoint = Point (rX shape + rWidth shape) (rY shape)
  mbNextBlock <- lookupBlockByPos nextPoint
  case mbNextBlock of
    Nothing -> return Nothing
    Just (nextBlockId, nextBlock) ->
      if rHeight nextBlock == rHeight shape
        then return $ Just (nextBlockId, nextBlock)
        else return Nothing

markMerged :: BlockId -> Color -> SolverM ()
markMerged blockId color = 
  modify $ \st -> st {ssMergedBlocks = M.insert blockId color (ssMergedBlocks st)}

removeMerged :: BlockId -> SolverM ()
removeMerged blockId =
  modify $ \st -> st {ssMergedBlocks = M.delete blockId (ssMergedBlocks st)}

lookupAvgColor :: BlockId -> SolverM (Maybe Color)
lookupAvgColor blockId = do
  avgColors <- gets ssAvgColorsByBlock
  return $ M.lookup blockId avgColors

lookupInitialColor :: Point -> SolverM Color
lookupInitialColor point = do
  cfg <- gets ssConfiguration
  return $ getColorAt cfg point

tryMergeRight :: BlockId -> Shape -> Color -> SolverM (Maybe BlockId)
tryMergeRight blockId block startColor = do
  mbBlock <- findBlockAtRight block
  trace (printf "find: %s -> %s\n" (show blockId) (show mbBlock)) $ return ()
  case mbBlock of
    Nothing -> do
      markMerged blockId startColor
      return Nothing
    Just (nextBlockId, nextBlock) -> do
      merged <- gets ssMergedBlocks
      mbNextColor <- case M.lookup nextBlockId merged of
                       Nothing -> do
                         mbAvgColor <- lookupAvgColor nextBlockId
                         case mbAvgColor of
                           Nothing -> return Nothing
                           Just avgColor -> do
                             initColor <- lookupInitialColor $ Point (rX nextBlock) (rY nextBlock)
                             if initColor == avgColor
                               then return Nothing
                               else return $ Just (avgColor, False)
                       Just mergedColor -> do
                         return $ Just (mergedColor, True)
      case mbNextColor of
        Nothing -> do
          markMerged blockId startColor
          return Nothing
        Just (nextColor, previouslyMerged) -> do
          if nextColor == startColor
            then do
              when previouslyMerged $
                removeMerged nextBlockId
              issueMove $ Merge blockId nextBlockId
              id <- lift $ gets isLastBlockId
              let newBlockId = createBlockId id
              markMerged newBlockId startColor
              return $ Just newBlockId
            else do
              markMerged blockId startColor
              return Nothing

tryMergeRightRecursive :: BlockId -> Shape -> Color -> SolverM (Maybe BlockId)
tryMergeRightRecursive blockId block startColor = do
  mbNext <- tryMergeRight blockId block startColor
  case mbNext of
    Nothing -> return Nothing
    Just nextBlockId -> do
      removeMerged blockId
      blocks <- lift $ gets isBlocks
      case HMS.lookup nextBlockId blocks of
        Nothing -> do
          trace (printf "block not found: %s -> %s" (show blockId) (show nextBlockId)) $ return ()
          return Nothing
        Just nextBlock -> do
          tryMergeRightRecursive nextBlockId nextBlock startColor

tryMergeAll :: SolverM ()
tryMergeAll = do
  blocks <- lift $ gets isBlocks
  merged <- gets ssMergedBlocks
  let freeBlockIds = filter (`M.notMember` merged) (HMS.keys blocks)
  if null freeBlockIds
    then return ()
    else do
      let nextBlockId = head freeBlockIds
          nextBlock = blocks HMS.! nextBlockId
      mbAvgColor <- lookupAvgColor nextBlockId
      case mbAvgColor of
        Nothing -> return ()
        Just avgColor -> do
          tryMergeRightRecursive nextBlockId nextBlock avgColor
          tryMergeAll

paintWithAvgColorsMergedS :: Configuration -> Image PixelRGBA8 -> SolverM ()
paintWithAvgColorsMergedS cfg img = do
  blocks <- lift $ gets (HMS.toList . isBlocks)
  let avgColorsByBlock = M.fromList [(blockId, calcAvgColor img block) | (blockId, block) <- blocks]
  modify $ \st -> st {ssAvgColorsByBlock = avgColorsByBlock}
  tryMergeAll
  merged <- gets ssMergedBlocks
  forM_ (M.toList merged) $ \(blockId, color) ->
    issueMove $ SetColor blockId color

paintWithAvgColorsMerged :: FilePath -> FilePath -> IO Program
paintWithAvgColorsMerged cfgPath imgPath = do
  img <- readPngImage imgPath
  cfg <- parseConfig cfgPath
  let intState = initialStateFromJson cfg
      solvState = initSolverState cfg
  return $ runSolver (paintWithAvgColorsMergedS cfg img) solvState intState

-- paintTest :: FilePath -> FilePath -> IO Program
-- paintTest cfgPath imgPath = do
--   img <- readPngImage imgPath
--   cfg <- parseConfig cfgPath
--   let intState = initialStateFromJson cfg
--       solvState = initSolverState
--   return $ runSolver (paintTestS cfg img) solvState intState

