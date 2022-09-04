
module Alt.DummySolver where

import Control.Monad
import Control.Monad.State
import Control.Parallel.Strategies
import Data.Maybe (mapMaybe)
import Data.List (minimumBy)
import Data.Function (on)
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
import Evaluator
import Alt.SolverM
import qualified Alt.Solver.Billboard as Billboard

colorRho :: Color -> Color -> Pixel8
colorRho (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
  maximum $ map abs [r1-r2, g1-g2, b1-b2, a1-a2]

colorTolerance :: Pixel8
colorTolerance = 2

paintWithAvgColors :: FilePath -> FilePath -> IO Program
paintWithAvgColors cfgPath imgPath = do
  img <- readPngImage imgPath
  cfg <- parseConfig cfgPath
  let blocksCfg = getColoredBlocks cfg
      paint (blockId, shape, existingColor) =
        let avgColor = calcAvgColor img shape
        in  if colorRho avgColor existingColor <= colorTolerance
              then Nothing
              else Just $ SetColor blockId avgColor
  return $ mapMaybe paint blocksCfg

findNextBlock :: Direction -> Shape -> SolverM (Maybe (BlockId, Shape))
findNextBlock ToRight shape = do
  let nextPoint = Point (rX shape + rWidth shape) (rY shape)
  mbNextBlock <- lookupBlockByPos nextPoint
  case mbNextBlock of
    Nothing -> return Nothing
    Just (nextBlockId, nextBlock) ->
      if rHeight nextBlock == rHeight shape
        then return $ Just (nextBlockId, nextBlock)
        else return Nothing
findNextBlock ToTop shape = do
  let nextPoint = Point (rX shape) (rY shape + rHeight shape)
  mbNextBlock <- lookupBlockByPos nextPoint
  case mbNextBlock of
    Nothing -> return Nothing
    Just (nextBlockId, nextBlock) ->
      if rWidth nextBlock == rWidth shape
        then return $ Just (nextBlockId, nextBlock)
        else return Nothing

mergeShapes :: Direction -> Shape -> Shape -> Shape
mergeShapes ToRight s1 s2 = mergeShapesHorizontal s1 s2
mergeShapes ToTop s1 s2 = mergeShapesVertical s1 s2

tryMerge :: Direction -> Point -> Shape -> Color -> SolverM (Maybe Shape)
tryMerge dir startPoint block startColor = do
  mbBlock <- findNextBlock dir block
  --when (dir == ToTop) $
  --  trace (printf "find: %s -> %s\n" (show block) (show mbBlock)) $ return ()
  case mbBlock of
    Nothing -> do
      markToBeMerged startPoint block startColor dir
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
                             initColor <- getInitialColor nextPoint
                             if colorRho initColor avgColor <= colorTolerance
                               then return Nothing
                               else return $ Just avgColor
                       Just mergedColor -> return Nothing
      case mbNextColor of
        Nothing -> do
          markToBeMerged startPoint block startColor dir
          return Nothing
        Just nextColor -> do
          -- trace (printf "start block %s, color %s; next block %s, color %s" (show block) (show startColor) (show nextBlockId) (show nextColor)) $ return ()
          if colorRho nextColor startColor <= colorTolerance
            then do
              mbArea <- lookupAreaToBeMerged $ Point (rX nextBlock) (rY nextBlock)
              case mbArea of
                Nothing -> do
                  let newShape = (mergeShapes dir block nextBlock)
                  --trace (printf "queue %s: %s + next block %s => %s" (show dir) (show block) (show nextBlock) (show newShape)) $ return ()
                  -- trace (printf "mark: %s => %s" (show startPoint) (show newShape)) $ return ()
                  markToBeMerged startPoint newShape startColor dir
                  return $ Just newShape
                Just (nextArea,_,_) -> do
                  let newShape = (mergeShapes dir block nextArea)
                  --trace (printf "queue %s: %s + next area %s => %s" (show dir) (show block) (show nextArea) (show newShape)) $ return ()
                  -- trace (printf "unmark: %s, mark: %s => %s" (show nextPoint) (show startPoint) (show newShape)) $ return ()
                  unmarkToBeMerged nextPoint
                  markToBeMerged startPoint newShape startColor dir
                  return $ Just newShape
            else do
              markToBeMerged startPoint block startColor dir
              return Nothing

tryMergeRecursive :: Direction -> Shape -> Color -> SolverM ()
tryMergeRecursive dir block startColor = do
  let startPoint = Point (rX block) (rY block)
  ok <- tryMerge dir startPoint block startColor
  case ok of
    Nothing -> return ()
    Just newShape -> do
      -- trace (printf "new: %s" (show newShape)) $ return ()
      tryMergeRecursive dir newShape startColor

tryMergeAll :: Direction -> SolverM ()
tryMergeAll dir = do
  blocksMap <- lift $ gets isBlocks
  let blocks = HMS.elems blocksMap
  -- trace (printf "all blocks: %s" (show $ HMS.keys blocksMap)) $ return ()
  mergeAreas <- gets ssAreasToMerge
  -- trace (printf "areas: %s" (show mergeAreas)) $ return ()
  let isFree block = not $ or [area `shapeContainsPoint` Point (rX block) (rY block) | (area,_,areaDir) <- M.elems mergeAreas, areaDir == dir]
  let freeBlocks = HMS.filter isFree blocksMap
      freeBlockIds = HMS.keys freeBlocks
  --when (dir == ToTop) $
  --  trace (printf "free blocks: %s" (show freeBlockIds)) $ return ()
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
          tryMergeRecursive dir nextBlock avgColor
          tryMergeAll dir

mergeAreaOneDirection :: Shape -> Direction -> Int -> Color -> SolverM Int
mergeAreaOneDirection area dir rowNumber color = do
    -- trace (printf "merging area: %s" (show area)) $ return ()
    mbFirstBlock <- lookupBlockByPos $ Point (rX area) (rY area + rowNumber)
    case mbFirstBlock of
      Nothing -> do
        -- trace (printf "first block not found: %s" (show area)) $ return ()
        return 0
      Just (firstBlockId, firstBlock) -> do
        mergeFrom 0 firstBlockId firstBlock
  where
    mergeFrom nDone firstBlockId firstBlock = do
      mbNext <- findNextBlock dir firstBlock
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
              mbNextFirst <- findNextBlock ToRight nextBlock
              case mbNextFirst of
                Nothing -> return (nDone+1)
                Just (nextFirstId, nextFirstBlock) -> do
                  let nextFirstStart = Point (rX nextFirstBlock) (rY nextFirstBlock)
                  if area `shapeContainsPoint` nextFirstStart
                    then mergeFrom (nDone+1) nextFirstId nextFirstBlock
                    else return nDone
            else return nDone

mergeAreaRows :: Shape -> Color -> SolverM Int
mergeAreaRows area color = do
  let rowNumbers = [rY area .. rY area + rHeight area - 1]
  results <- forM rowNumbers $ \rowNumber ->
               mergeAreaOneDirection area ToRight rowNumber color
  return $ sum results

mergeAreaOnce :: Shape -> Color -> SolverM Int
mergeAreaOnce area color = do
  byRows <- mergeAreaRows area color
  byColumn <- mergeAreaOneDirection area ToTop 0 color
  return $ byRows + byColumn

mergeAreaRecursive :: Shape -> Color -> SolverM ()
mergeAreaRecursive area color = do
  done <- mergeAreaOnce area color
  if done > 0
    then mergeAreaRecursive area color
    else return ()

mergeAllAreas :: SolverM ()
mergeAllAreas = do
  areas <- gets ssAreasToMerge
  forM_ (M.elems areas) $ \(area,color,_) -> do
    --trace (printf "merging area: %s" (show area)) $ return ()
    mergeAreaRecursive area color

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

swapAvgColors :: BlockId -> BlockId -> SolverM ()
swapAvgColors b1 b2 = do
  modify $ \st -> st {
      ssAvgColorsByBlock = M.insert b1 (ssAvgColorsByBlock st M.! b2) $
                            M.insert b2 (ssAvgColorsByBlock st M.! b1) (ssAvgColorsByBlock st)
    }

paintWithAvgColorsMerged :: Configuration -> Image PixelRGBA8 -> SolverM ()
paintWithAvgColorsMerged cfg img = do
  rememberAvgColors img
  -- checkSwapAllOnce
  tryMergeAll ToRight
  mergeAllAreas
  paintMergedBlocks

cutToQuads :: Int -> BlockId -> SolverM ()
cutToQuads 0 _ = return ()
cutToQuads level blockId = do
  block <- lift $ getBlock blockId
  if rWidth block > 1 && rHeight block > 1
    then do
      let middleX = rX block + (rWidth block `div` 2)
          middleY = rY block + (rHeight block `div` 2)
          middle = Point middleX middleY
      issueMove $ PointCut blockId middle
      let children = [blockId +. i | i <- [0..3]]
      forM_ children $ \child -> cutToQuads (level-1) child
    else return ()

paintByQuadsAndMerge :: Int -> Image PixelRGBA8 -> SolverM ()
paintByQuadsAndMerge level img = do
  cutToQuads level (createBlockId 0)
  rememberAvgColors img
  tryMergeAll ToRight
  tryMergeAll ToTop
  mergeAllAreas
  paintMergedBlocks

searchBestCutPoint :: Image PixelRGBA8 -> BlockId -> Shape -> SolverM [BlockId]
searchBestCutPoint img blockId root = do
  let integralImg = makeIntegralImage img
      checkPoint p =
        let quads = cutPointShape root p
            subAvgColors = parMap rpar (calcAvgColorFromIntegral integralImg) quads
            subRhos = parMap rpar (\(quad, avg) -> imagePartDeviation img quad avg) (zip quads subAvgColors)
        in  (p, sum subRhos, quads, subAvgColors)
      step = 10
      allPoints = [Point x y | x <- [rX root + 1, rX root + step + 1 .. rWidth root - 2],
                               y <- [rY root + 1, rY root + step + 1 .. rHeight root - 2]]
      checkResults = map checkPoint allPoints
      (bestPoint, _, bestQuads, avgColors) = minimumBy (compare `on` \(_,rho,_,_) -> rho) checkResults
  issueMove $ PointCut blockId bestPoint
  let children = [blockId +. i | i <- [0..3]]
  return children

paintByQuadsSearchAndMerge :: Int -> Image PixelRGBA8 -> SolverM ()
paintByQuadsSearchAndMerge level img = do
  let root = Rectangle 0 0 (imageWidth img) (imageHeight img)
  initQuads <- searchBestCutPoint img (createBlockId 0) root
  forM_ initQuads $ \quad -> cutToQuads (level-1) quad
  rememberAvgColors img
  tryMergeAll ToRight
  tryMergeAll ToTop
  mergeAllAreas
  paintMergedBlocks

paintByQuadsSearchBillboard :: Image PixelRGBA8 -> SolverM ()
paintByQuadsSearchBillboard img = do
  let root = Rectangle 0 0 (imageWidth img) (imageHeight img)
  initQuads <- searchBestCutPoint img (createBlockId 0) root
  forM_ initQuads $ \quadId -> do
    quad <- lift $ getBlock quadId
    Billboard.solveInside img (quadId, quad)

solveRecursiveAndMerge :: Image PixelRGBA8 -> SolverM ()
solveRecursiveAndMerge img = do
    let root = Rectangle 0 0 (imageWidth img) (imageHeight img)
    quads <- searchBestCutPoint img (createBlockId 0) root
    forM_ quads subdivide
    rememberAvgColors img
    tryMergeAll ToRight
    tryMergeAll ToTop
    mergeAllAreas
    paintMergedBlocks
  where 
    integralImg = makeIntegralImage img

    subdivide blockId = do
      block <- lift $ getBlock blockId
      let commonAvgColor = calcAvgColorFromIntegral integralImg block
      if rWidth block <= 50 || rHeight block <= 50
        then return ()
          -- avgColor <- getAvgColor blockId
          -- issueMove $ SetColor blockId avgColor
        else do
          let middleX = rX block + (rWidth block `div` 2)
              middleY = rY block + (rHeight block `div` 2)
              middle = Point middleX middleY
              quads = cutPointShape block middle
              halfsX = cutVerticalShape block middleX
              halfsY = cutHorizontalShape block middleY
          let commonRho = imagePartDeviation img block commonAvgColor
              quadsAvgColors = [calcAvgColorFromIntegral integralImg child | child <- quads]
              quadRhos = sum [imagePartDeviation img child avg | (child, avg) <- zip quads quadsAvgColors]
              halfsXAvgColors = [calcAvgColorFromIntegral integralImg child | child <- halfsX]
              halfsYAvgColors = [calcAvgColorFromIntegral integralImg child | child <- halfsY]
              halfsXRhos = sum [imagePartDeviation img child avg | (child, avg) <- zip halfsX halfsXAvgColors]
              halfsYRhos = sum [imagePartDeviation img child avg | (child, avg) <- zip halfsY halfsYAvgColors]
              minRho = minimum [quadRhos, halfsXRhos, halfsYRhos]
          trace (printf "common %f, Q %f, X %f, Y %f" commonRho quadRhos halfsXRhos halfsYRhos) $ return ()
          if minRho >= commonRho
            then return ()
            else if minRho == quadRhos
                   then do
                     issueMove $ PointCut blockId middle
                     let childBlockIds = [blockId +. i | i <- [0..3]]
                     forM_ childBlockIds $ \child -> subdivide child
                   else if minRho == halfsYRhos
                          then do
                            issueMove $ LineCut blockId Horizontal middleY
                            let childBlockIds = [blockId +. i | i <- [0,1]]
                            forM_ childBlockIds $ \child -> subdivide child
                          else do
                            issueMove $ LineCut blockId Vertical middleX
                            let childBlockIds = [blockId +. i | i <- [0,1]]
                            forM_ childBlockIds $ \child -> subdivide child

checkSwap :: BlockId -> SolverM Bool
checkSwap blockId = do
  block <- lift $ getBlock blockId
  mbNext <- findNextBlock ToRight block
  case mbNext of
    Nothing -> return False
    Just (nextBlockId, nextBlock) -> do
      avgColor1 <- getAvgColor blockId
      avgColor2 <- getAvgColor nextBlockId
      initColor1 <- calcInitialAvgColor blockId
      initColor2 <- calcInitialAvgColor nextBlockId
      if (colorRho avgColor1 initColor2 > colorRho avgColor1 initColor1) &&
            (colorRho avgColor2 initColor1 > colorRho avgColor2 initColor2)
        then do
          issueMove $ Swap blockId nextBlockId
          swapAvgColors blockId nextBlockId
          return True
        else return False

checkSwapAllOnce :: SolverM Int
checkSwapAllOnce = do
    blocksMap <- lift $ gets isBlocks
    go (HMS.keys blocksMap)
  where
    go [] = return 0
    go (blockId : rest) = do
      ok <- checkSwap blockId
      if ok
        then (1+) <$> go rest
        else go rest

checkSwapAll :: SolverM ()
checkSwapAll = do
  done <- checkSwapAllOnce
  trace (printf "done swaps: %d" done) $ return ()
  if done > 0
    then checkSwapAll
    else return ()

