
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
import ShapeUtils

data SolverState = SolverState {
      ssProgram :: Program
    , ssAreasToMerge :: M.Map Point (Shape, Color)
    , ssMergedBlocks :: M.Map BlockId Color
    , ssAvgColorsByBlock :: M.Map BlockId Color
    , ssConfiguration :: Configuration
  }

initSolverState :: Configuration -> SolverState
initSolverState cfg = SolverState [] M.empty M.empty M.empty cfg

type SolverM a = StateT SolverState (State InterpreterState) a

runSolver :: SolverM () -> SolverState -> InterpreterState -> Program
runSolver solver st ist =
  let interpret = execStateT solver st
      st' = evalState interpret ist
  in  reverse $ ssProgram st'

runSolverM :: FilePath -> FilePath -> (Configuration -> Image PixelRGBA8 -> SolverM ()) -> IO Program
runSolverM cfgPath imgPath solver = do
  img <- readPngImage imgPath
  cfg <- parseConfig cfgPath
  let intState = initialStateFromJson cfg
      solvState = initSolverState cfg
  return $ runSolver (solver cfg img) solvState intState

runSolverSimpleM :: FilePath -> (Image PixelRGBA8 -> SolverM ()) -> IO Program
runSolverSimpleM imgPath solver = do
  img <- readPngImage imgPath
  let size = (imageWidth img, imageHeight img)
      initState = initialState size
      cfg = emptyConfiguration size
      solvState = initSolverState cfg
  return $ runSolver (solver img) solvState initState

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

markToBeMerged :: Point -> Shape -> Color -> SolverM ()
markToBeMerged point shape color =
  modify $ \st -> st {ssAreasToMerge = M.insert point (shape,color) (ssAreasToMerge st)}

unmarkToBeMerged :: Point -> SolverM ()
unmarkToBeMerged point =
  modify $ \st -> st {ssAreasToMerge = M.delete point (ssAreasToMerge st)}

replaceToBeMerged :: Point -> Point -> Shape -> Color -> SolverM ()
replaceToBeMerged oldPoint newPoint shape color = do
  modify $ \st -> st {ssAreasToMerge = M.insert newPoint (shape,color) $ M.delete oldPoint (ssAreasToMerge st)}

lookupAvgColor :: BlockId -> SolverM (Maybe Color)
lookupAvgColor blockId = do
  avgColors <- gets ssAvgColorsByBlock
  return $ M.lookup blockId avgColors

lookupInitialColor :: Point -> SolverM Color
lookupInitialColor point = do
  cfg <- gets ssConfiguration
  return $ getColorAt cfg point

lookupAreaToBeMerged :: Point -> SolverM (Maybe (Shape, Color))
lookupAreaToBeMerged point = do
  areas <- gets ssAreasToMerge
  return $ M.lookup point areas

tryMergeRight :: Point -> Shape -> Color -> SolverM (Maybe Shape)
tryMergeRight startPoint block startColor = do
  mbBlock <- findBlockAtRight block
  -- trace (printf "find: %s -> %s\n" (show block) (show mbBlock)) $ return ()
  case mbBlock of
    Nothing -> do
      markToBeMerged startPoint block startColor
      return Nothing
    Just (nextBlockId, nextBlock) -> do
      let nextPoint = Point (rX nextBlock) (rY nextBlock)
      merged <- gets ssMergedBlocks
      mbNextColor <- case M.lookup nextBlockId merged of
                       Nothing -> do
                         mbAvgColor <- lookupAvgColor nextBlockId
                         case mbAvgColor of
                           Nothing -> return Nothing
                           Just avgColor -> do
                             initColor <- lookupInitialColor nextPoint
                             if initColor == avgColor
                               then return Nothing
                               else return $ Just avgColor
                       Just mergedColor -> return Nothing
      case mbNextColor of
        Nothing -> do
          markToBeMerged startPoint block startColor
          return Nothing
        Just nextColor -> do
          -- trace (printf "start block %s, color %s; next block %s, color %s" (show block) (show startColor) (show nextBlockId) (show nextColor)) $ return ()
          if nextColor == startColor
            then do
              mbArea <- lookupAreaToBeMerged $ Point (rX nextBlock) (rY nextBlock)
              case mbArea of
                Nothing -> do
                  let newShape = (mergeShapesHorizontal block nextBlock)
                  -- trace (printf "queue: %s + next block %s => %s" (show block) (show nextBlock) (show newShape)) $ return ()
                  -- trace (printf "mark: %s => %s" (show startPoint) (show newShape)) $ return ()
                  markToBeMerged startPoint newShape startColor
                  return $ Just newShape
                Just (nextArea,_) -> do
                  let newShape = (mergeShapesHorizontal block nextArea)
                  -- trace (printf "queue: %s + next area %s => %s" (show block) (show nextArea) (show newShape)) $ return ()
                  -- trace (printf "unmark: %s, mark: %s => %s" (show nextPoint) (show startPoint) (show newShape)) $ return ()
                  unmarkToBeMerged nextPoint
                  markToBeMerged startPoint newShape startColor
                  return $ Just newShape
            else do
              markToBeMerged startPoint block startColor
              return Nothing

tryMergeRightRecursive :: Shape -> Color -> SolverM ()
tryMergeRightRecursive block startColor = do
  let startPoint = Point (rX block) (rY block)
  ok <- tryMergeRight startPoint block startColor
  case ok of
    Nothing -> return ()
    Just newShape -> do
      -- trace (printf "new: %s" (show newShape)) $ return ()
      tryMergeRightRecursive newShape startColor

tryMergeAll :: SolverM ()
tryMergeAll = do
  blocksMap <- lift $ gets isBlocks
  let blocks = HMS.elems blocksMap
  -- trace (printf "all blocks: %s" (show $ HMS.keys blocksMap)) $ return ()
  mergeAreas <- gets ssAreasToMerge
  -- trace (printf "areas: %s" (show mergeAreas)) $ return ()
  let isFree block = not $ or [area `shapeContainsPoint` Point (rX block) (rY block) | (area,_) <- M.elems mergeAreas]
  let freeBlocks = HMS.filter isFree blocksMap
      freeBlockIds = HMS.keys freeBlocks
  -- trace (printf "free blocks: %s" (show freeBlockIds)) $ return ()
  if null freeBlockIds
    then return ()
    else do
      let nextBlockId = head freeBlockIds
          nextBlock = freeBlocks HMS.! nextBlockId
      mbAvgColor <- lookupAvgColor nextBlockId
      case mbAvgColor of
        Nothing -> do
          trace (printf "Unknown avg color at %s" (show nextBlockId)) $ return ()
          return ()
        Just avgColor -> do
          tryMergeRightRecursive nextBlock avgColor
          tryMergeAll

mergeAreaOnce :: Shape -> Color -> SolverM Int
mergeAreaOnce area color = do
    -- trace (printf "merging area: %s" (show area)) $ return ()
    mbFirstBlock <- lookupBlockByPos $ Point (rX area) (rY area)
    case mbFirstBlock of
      Nothing -> do
        -- trace (printf "first block not found: %s" (show area)) $ return ()
        return 0
      Just (firstBlockId, firstBlock) -> do
        mergeFrom 0 firstBlockId firstBlock
  where
    mergeFrom nDone firstBlockId firstBlock = do
      mbNext <- findBlockAtRight firstBlock
      case mbNext of
        Nothing -> return nDone
        Just (nextBlockId, nextBlock) -> do
          let nextStart = Point (rX nextBlock) (rY nextBlock)
          if area `shapeContainsPoint` nextStart
            then do
              -- trace (printf "area %s, next %s" (show area) (show nextBlock)) $ return ()
              removeMerged nextBlockId
              removeMerged firstBlockId
              -- trace (printf "merge: %s + %s" (show firstBlock) (show nextBlock)) $ return ()
              issueMove $ Merge firstBlockId nextBlockId
              id <-lift $ gets isLastBlockId
              let newBlockId = createBlockId id
              markMerged newBlockId color
              mbNextFirst <- findBlockAtRight nextBlock
              case mbNextFirst of
                Nothing -> return (nDone+1)
                Just (nextFirstId, nextFirstBlock) -> do
                  let nextFirstStart = Point (rX nextFirstBlock) (rY nextFirstBlock)
                  if area `shapeContainsPoint` nextFirstStart
                    then mergeFrom (nDone+1) nextFirstId nextFirstBlock
                    else return nDone
            else return nDone
          
mergeAreaRecursive :: Shape -> Color -> SolverM ()
mergeAreaRecursive area color = do
  done <- mergeAreaOnce area color
  if done > 0
    then mergeAreaRecursive area color
    else return ()

mergeAllAreas :: SolverM ()
mergeAllAreas = do
  areas <- gets ssAreasToMerge
  forM_ (M.elems areas) $ \(area,color) ->
    mergeAreaRecursive area color

calcInitialAvgColor :: BlockId -> SolverM Color
calcInitialAvgColor blockId = do
  block <- lift $ getBlock blockId
  cfg <- gets ssConfiguration
  return $ calcAvgColorFromConfig cfg block

paintMergedBlocks :: SolverM ()
paintMergedBlocks = do
  merged <- gets ssMergedBlocks
  forM_ (M.toList merged) $ \(blockId, color) -> do
    initColor <- calcInitialAvgColor blockId
    when (initColor /= color) $
      issueMove $ SetColor blockId color
  blocksMap <- lift $ gets isBlocks
  let unmergedIds = filter (`M.notMember` merged) (HMS.keys blocksMap)
  forM_ unmergedIds $ \blockId -> do
    block <- lift $ getBlock blockId
    mbColor <- lookupAvgColor blockId
    case mbColor of
      Nothing -> return ()
      Just color -> do
        initColor <- calcInitialAvgColor blockId
        when (initColor /= color) $
          issueMove $ SetColor blockId color

rememberAvgColors :: Image PixelRGBA8 -> SolverM ()
rememberAvgColors img = do
  blocks <- lift $ gets (HMS.toList . isBlocks)
  let avgColorsByBlock = M.fromList [(blockId, calcAvgColor img block) | (blockId, block) <- blocks]
  -- trace (printf "avg colors: %s" (show avgColorsByBlock)) $ return ()
  modify $ \st -> st {ssAvgColorsByBlock = avgColorsByBlock}

paintWithAvgColorsMerged :: Configuration -> Image PixelRGBA8 -> SolverM ()
paintWithAvgColorsMerged cfg img = do
  rememberAvgColors img
  tryMergeAll
  mergeAllAreas
  paintMergedBlocks

cutToQuads :: Int -> BlockId -> SolverM ()
cutToQuads 0 _ = return ()
cutToQuads level blockId = do
  block <- lift $ getBlock blockId
  let middleX = rX block + (rWidth block `div` 2)
      middleY = rY block + (rHeight block `div` 2)
      middle = Point middleX middleY
  issueMove $ PointCut blockId middle
  let children = [blockId +. i | i <- [0..3]]
  forM_ children $ \child -> cutToQuads (level-1) child

paintByQuadsAndMerge :: Int -> Image PixelRGBA8 -> SolverM ()
paintByQuadsAndMerge level img = do
  cutToQuads level (createBlockId 0)
  rememberAvgColors img
  tryMergeAll
  mergeAllAreas
  paintMergedBlocks

-- paintTest :: FilePath -> FilePath -> IO Program
-- paintTest cfgPath imgPath = do
--   img <- readPngImage imgPath
--   cfg <- parseConfig cfgPath
--   let intState = initialStateFromJson cfg
--       solvState = initSolverState
--   return $ runSolver (paintTestS cfg img) solvState intState

