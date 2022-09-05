
module Alt.SolverM where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HMS

import Codec.Picture.Types

import Types
import PNG
import Alt.AST
import Alt.Interpreter
import Json

data Direction = ToRight | ToTop
  deriving (Eq, Show)

data SolverState = SolverState {
      ssProgram :: Program
    , ssAreasToMerge :: M.Map Point (Shape, Color, Direction)
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

-- | Execute some code and forget state changes;
-- return the state which you would get if the code was actually executed.
doAndRollback :: SolverM a -> SolverM (a, SolverState, InterpreterState)
doAndRollback solver = do
  solverSt <- get
  interpSt <- lift get
  result <- solver
  solverSt' <- get
  interpSt' <- lift get
  put solverSt
  lift $ put interpSt
  return (result, solverSt', interpSt')

-- | Issue a command and interpret it
issueMove :: Move -> SolverM ()
issueMove m = do
  lift $ interpretMove m
  modify $ \st -> st {ssProgram = m : ssProgram st}

getBlocksByPos :: SolverM (M.Map Point (BlockId, Shape))
getBlocksByPos = do
  blocks <- lift $ gets isBlocks
  return $ M.fromList [(Point (rX block) (rY block), (blockId, block)) | (blockId, block) <- HMS.toList blocks]

-- | Get block by position of its bottom-left point
lookupBlockByPos :: Point -> SolverM (Maybe (BlockId, Shape))
lookupBlockByPos point = do
  blocksByPos <- getBlocksByPos
  return $ M.lookup point blocksByPos

markMerged :: BlockId -> Color -> SolverM ()
markMerged blockId color =
  modify $ \st -> st {ssMergedBlocks = M.insert blockId color (ssMergedBlocks st)}

removeMerged :: BlockId -> SolverM ()
removeMerged blockId =
  modify $ \st -> st {ssMergedBlocks = M.delete blockId (ssMergedBlocks st)}

markToBeMerged :: Point -> Shape -> Color -> Direction -> SolverM ()
markToBeMerged point shape color dir =
  modify $ \st -> st {ssAreasToMerge = M.insert point (shape,color,dir) (ssAreasToMerge st)}

unmarkToBeMerged :: Point -> SolverM ()
unmarkToBeMerged point =
  modify $ \st -> st {ssAreasToMerge = M.delete point (ssAreasToMerge st)}

replaceToBeMerged :: Point -> Point -> Shape -> Color -> Direction -> SolverM ()
replaceToBeMerged oldPoint newPoint shape color dir = do
  modify $ \st -> st {ssAreasToMerge = M.insert newPoint (shape,color,dir) $ M.delete oldPoint (ssAreasToMerge st)}

-- | Get average color from corresponding area on the target image
-- (must be remembered by rememberAvgColors first)
lookupAvgColor :: BlockId -> SolverM (Maybe Color)
lookupAvgColor blockId = do
  avgColors <- gets ssAvgColorsByBlock
  return $ M.lookup blockId avgColors

-- | Get average color from corresponding area on the target image
-- (must be remembered by rememberAvgColors first)
getAvgColor :: BlockId -> SolverM Color
getAvgColor blockId = do
  r <- lookupAvgColor blockId
  case r of
    Nothing -> error $ "No average color for block " ++ show blockId
    Just color -> return color

-- | Get color from initial configuration
getInitialColor :: Point -> SolverM Color
getInitialColor point = do
  cfg <- gets ssConfiguration
  return $ getColorAt cfg point

lookupAreaToBeMerged :: Point -> SolverM (Maybe (Shape, Color, Direction))
lookupAreaToBeMerged point = do
  areas <- gets ssAreasToMerge
  return $ M.lookup point areas

-- | Calculate average color on the area on initial configuration
calcInitialAvgColor :: BlockId -> SolverM Color
calcInitialAvgColor blockId = do
  block <- lift $ getBlock blockId
  cfg <- gets ssConfiguration
  return $ calcAvgColorFromConfig cfg block

-- | For each existing block, remember average color of corresponding area on the target image
rememberAvgColors :: Image PixelRGBA8 -> SolverM ()
rememberAvgColors img = do
  blocks <- lift $ gets (HMS.toList . isBlocks)
  let integralImg = makeIntegralImage img
  let avgColorsByBlock = M.fromList [(blockId, calcAvgColorFromIntegral integralImg block) | (blockId, block) <- blocks]
  -- trace (printf "avg colors: %s" (show avgColorsByBlock)) $ return ()
  modify $ \st -> st {ssAvgColorsByBlock = avgColorsByBlock}

