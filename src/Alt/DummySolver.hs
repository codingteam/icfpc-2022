
module Alt.DummySolver where

import Control.Monad
import Control.Monad.State
import Control.Parallel.Strategies
import Data.Maybe (mapMaybe)
import Data.List (minimumBy)
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V
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
    Nothing -> do
      -- trace (printf "find: no block at %s" (show nextPoint)) $ return ()
      return Nothing
    Just (nextBlockId, nextBlock) ->
      if rWidth nextBlock == rWidth shape
        then return $ Just (nextBlockId, nextBlock)
        else do
          -- trace (printf "find: bad block: %s" (show nextBlock)) $ return ()
          return Nothing

mergeShapes :: Direction -> Shape -> Shape -> Shape
mergeShapes ToRight s1 s2 = mergeShapesHorizontal s1 s2
mergeShapes ToTop s1 s2 = mergeShapesVertical s1 s2

tryMerge :: Direction -> Point -> Shape -> Color -> Pixel8 -> SolverM (Maybe Shape)
tryMerge dir startPoint block startColor tolerance = do
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
                             if colorRho initColor avgColor <= tolerance
                               then return Nothing
                               else return $ Just avgColor
                       Just mergedColor -> return Nothing
      case mbNextColor of
        Nothing -> do
          markToBeMerged startPoint block startColor dir
          return Nothing
        Just nextColor -> do
          -- trace (printf "start block %s, color %s; next block %s, color %s" (show block) (show startColor) (show nextBlockId) (show nextColor)) $ return ()
          if colorRho nextColor startColor <= tolerance
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

tryMergeRecursive :: Direction -> Shape -> Color -> Pixel8 -> SolverM ()
tryMergeRecursive dir block startColor tolerance = do
  let startPoint = Point (rX block) (rY block)
  ok <- tryMerge dir startPoint block startColor tolerance
  case ok of
    Nothing -> return ()
    Just newShape -> do
      -- trace (printf "new: %s" (show newShape)) $ return ()
      tryMergeRecursive dir newShape startColor tolerance

tryMergeAll :: Direction -> Pixel8 -> SolverM ()
tryMergeAll dir tolerance = do
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
          tryMergeRecursive dir nextBlock avgColor tolerance
          tryMergeAll dir tolerance

mergeAreaOneDirection :: Shape -> Direction -> Int -> Color -> SolverM Int
mergeAreaOneDirection area dir rowNumber color = do
    -- trace (printf "merging area: %s" (show area)) $ return ()
    mbFirstBlock <- findFirstBlock
    case mbFirstBlock of
      Nothing -> do
        -- trace (printf "first block not found: %s" (show area)) $ return ()
        return 0
      Just (firstBlockId, firstBlock) -> do
        -- trace (printf "%s: first block #%s - %s" (show dir) (show rowNumber) (show firstBlock)) $ return ()
        mergeFrom 0 firstBlockId firstBlock
  where
    findFirstBlock
      | dir == ToTop = lookupBlockByPos $ Point (rX area) (rY area)
      | otherwise = do
        mbFirst <- lookupBlockByPos $ Point (rX area) (rY area)
        case mbFirst of
          Nothing -> return Nothing
          Just (firstBlockId, firstBlock) -> stepUp rowNumber (firstBlockId, firstBlock)

    stepUp :: Int -> (BlockId,Shape) -> SolverM (Maybe (BlockId, Shape))
    stepUp 0 (blockId,block) = return (Just (blockId,block))
    stepUp n (_,block) = do
      mbNext <- findAnyUpperBlock block
      case mbNext of
        Just (nextBlockId,nextBlock) -> stepUp (n-1) (nextBlockId,nextBlock)
        Nothing -> return Nothing

    findAnyUpperBlock block = do
      let nextPoint = Point (rX block) (rY block + rHeight block)
      lookupBlockByPos nextPoint

    mergeFrom nDone firstBlockId firstBlock = do
      mbNext <- findNextBlock dir firstBlock
      case mbNext of
        Nothing -> return nDone
        Just (nextBlockId, nextBlock) -> do
          -- when (dir == ToTop) $
          --   trace (printf "next: %s" (show nextBlock)) $ return ()
          let nextStart = Point (rX nextBlock) (rY nextBlock)
          if area `shapeContainsPoint` nextStart
            then do
              -- when (dir == ToTop) $
                -- trace (printf "area %s, next %s" (show firstBlock) (show nextBlock)) $ return ()
              removeMerged nextBlockId
              removeMerged firstBlockId
              -- trace (printf "merge %s: %s + %s" (show dir) (show firstBlock) (show nextBlock)) $ return ()
              issueMove $ Merge firstBlockId nextBlockId
              id <-lift $ gets isLastBlockId
              -- trace (printf "merge %s: %s + %s => %s" (show dir) (show firstBlockId) (show nextBlockId) (show id)) $ return ()
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

mergeAreaRowsRecursive :: Shape -> Color -> SolverM ()
mergeAreaRowsRecursive area color = do
  done <- mergeAreaRows area color
  if done > 0
    then mergeAreaRowsRecursive area color
    else return ()

mergeAreaColumnsRecursive :: Shape -> Color -> SolverM ()
mergeAreaColumnsRecursive area color = do
  done <- mergeAreaOneDirection area ToTop 0 color
  if done > 0
    then mergeAreaColumnsRecursive area color
    else return ()

mergeAllAreas :: SolverM ()
mergeAllAreas = do
  areas <- gets ssAreasToMerge
  forM_ (M.elems areas) $ \(area,color,_) -> do
    --trace (printf "merging area: %s" (show area)) $ return ()
    mergeAreaRowsRecursive area color
    mergeAreaColumnsRecursive area color

paintMergedBlocks :: Pixel8 -> SolverM ()
paintMergedBlocks tolerance = do
  merged <- gets ssMergedBlocks
  forM_ (M.toList merged) $ \(blockId, color) -> do
    initColor <- calcInitialAvgColor blockId
    when (colorRho initColor color > tolerance) $ do
      -- trace (printf "color.area %s %s" (show blockId) (show color)) $ return ()
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
        when (colorRho initColor color > tolerance) $ do
          -- trace (printf "color.unmerged %s %s" (show blockId) (show color)) $ return ()
          issueMove $ SetColor blockId color

swapAvgColors :: BlockId -> BlockId -> SolverM ()
swapAvgColors b1 b2 = do
  modify $ \st -> st {
      ssAvgColorsByBlock = M.insert b1 (ssAvgColorsByBlock st M.! b2) $
                            M.insert b2 (ssAvgColorsByBlock st M.! b1) (ssAvgColorsByBlock st)
    }

paintWithAvgColorsMerged :: Pixel8 -> Configuration -> Image PixelRGBA8 -> SolverM ()
paintWithAvgColorsMerged tolerance cfg img = do
  rememberAvgColors img
  -- checkSwapAllOnce
  tryMergeAll ToRight tolerance
  tryMergeAll ToTop tolerance
  mergeAllAreas
  paintMergedBlocks tolerance

mergeAll :: Color -> SolverM ()
mergeAll color = do
  (w,h) <- lift getImageSize
  let root = Rectangle 0 0 w h
  modify $ \st -> st {ssAreasToMerge = M.singleton (Point 0 0) (root, color, ToTop)}
  mergeAllAreas

repaintByQuadsAndMerge :: Int -> Pixel8 -> Image PixelRGBA8 -> SolverM ()
repaintByQuadsAndMerge level tolerance img = do
  let root = Rectangle 0 0 (imageWidth img) (imageHeight img)
  let avgColor = calcAvgColor img root
  mergeAll avgColor
  modify $ \st -> st {ssAreasToMerge = M.empty}
--   Billboard.mergeAllBlocks <$> gets ssConfiguration
  id <-lift $ gets isLastBlockId
  let blockId = (createBlockId id)
  paintByQuadsAndMerge' level tolerance img blockId

repaintByQuadsSearchBillboard :: Image PixelRGBA8 -> SolverM ()
repaintByQuadsSearchBillboard img = do
  let root = Rectangle 0 0 (imageWidth img) (imageHeight img)
  let avgColor = calcAvgColor img root
  mergeAll avgColor
  modify $ \st -> st {ssAreasToMerge = M.empty}
--   Billboard.mergeAllBlocks <$> gets ssConfiguration
  id <-lift $ gets isLastBlockId
  let blockId = (createBlockId id)
  paintByQuadsSearchBillboard' img blockId

repaintByQuadsSearchAndMerge :: Int -> Pixel8 -> Image PixelRGBA8 -> SolverM ()
repaintByQuadsSearchAndMerge level tolerance img = do
  let root = Rectangle 0 0 (imageWidth img) (imageHeight img)
  let avgColor = calcAvgColor img root
  mergeAll avgColor
  modify $ \st -> st {ssAreasToMerge = M.empty}
--   Billboard.mergeAllBlocks <$> gets ssConfiguration
  id <-lift $ gets isLastBlockId
  let blockId = (createBlockId id)
  paintByQuadsSearchAndMerge' level tolerance img blockId

repaintRecursiveHalfs :: Int -> Orientation -> Image PixelRGBA8 -> SolverM ()
repaintRecursiveHalfs level dir img = do
  let root = Rectangle 0 0 (imageWidth img) (imageHeight img)
  let avgColor = calcAvgColor img root
  mergeAll avgColor
  modify $ \st -> st {ssAreasToMerge = M.empty}
  id <-lift $ gets isLastBlockId
  let blockId = (createBlockId id)
  recursiveHalfs2' level dir img blockId (createBlockId $ id+1)

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
      removeMerged blockId
      let children = [blockId +. i | i <- [0..3]]
      forM_ children $ \child -> cutToQuads (level-1) child
    else return ()

paintByQuadsAndMerge' :: Int -> Pixel8 -> Image PixelRGBA8 -> BlockId -> SolverM ()
paintByQuadsAndMerge' level tolerance img blockId = do
  cutToQuads level blockId
  rememberAvgColors img
  tryMergeAll ToRight tolerance
  tryMergeAll ToTop tolerance
  mergeAllAreas
  paintMergedBlocks tolerance

paintByQuadsAndMerge :: Int -> Pixel8 -> Image PixelRGBA8 -> SolverM ()
paintByQuadsAndMerge level tolerance img = paintByQuadsAndMerge' level tolerance img (createBlockId 0)

searchBestCutPoint :: Image PixelRGBA8 -> BlockId -> Shape -> SolverM [BlockId]
searchBestCutPoint img blockId root = do
  let integralImg = makeIntegralImage img
      checkPoint p =
        let quads = cutPointShape root p
            subAvgColors = map (calcAvgColorFromIntegral integralImg) quads
            subRhos = map (\(quad, avg) -> imagePartDeviation img quad avg) (zip quads subAvgColors)
        in  (p, sum subRhos, quads, subAvgColors)
      step = 10
      allPoints = [Point x y | x <- [rX root + 1, rX root + step + 1 .. rX root + rWidth root - 2],
                               y <- [rY root + 1, rY root + step + 1 .. rY root + rHeight root - 2]]
  let checkResults = parMap rpar checkPoint allPoints
  let (bestPoint, _, bestQuads, avgColors) = minimumBy (compare `on` \(_,rho,_,_) -> rho) $ checkResults
  issueMove $ PointCut blockId bestPoint
  removeMerged blockId
  let children = [blockId +. i | i <- [0..3]]
  return children

paintByQuadsSearch2 :: Int -> Image PixelRGBA8 -> SolverM ()
paintByQuadsSearch2 level img = do
    let blockId = createBlockId 0
    root <- lift $ getBlock blockId
    let middleX = rX root + (rWidth root `div` 2)
        middleY = rY root + (rHeight root `div` 2)
        middle = Point middleX middleY
    issueMove $ PointCut blockId middle
    removeMerged blockId
    let quads = [blockId +. i | i <- [0..3]]
    forM_ quads $ \quadId -> do
      quad <- lift $ getBlock quadId
      subQuads <- searchBestCutPoint img quadId quad
      forM_ subQuads $ \subQuadId -> do
        cutToQuads (level-2) subQuadId
    rememberAvgColors img
    let tolerance = 2
    tryMergeAll ToRight tolerance
    tryMergeAll ToTop tolerance
    mergeAllAreas
    paintMergedBlocks tolerance
--     blockIds <- lift $ gets (HMS.keys . isBlocks)
--     forM_ blockIds $ \blockId -> do
--       avgColor <- getAvgColor blockId
--       issueMove $ SetColor blockId avgColor

paintByQuadsSearchAndMerge :: Int -> Pixel8 -> Image PixelRGBA8 -> SolverM ()
paintByQuadsSearchAndMerge level tolerance img = paintByQuadsSearchAndMerge' level tolerance img (createBlockId 0)

paintByQuadsSearchAndMerge' :: Int -> Pixel8 -> Image PixelRGBA8 -> BlockId -> SolverM ()
paintByQuadsSearchAndMerge' level tolerance img blockId = do
  root <- lift $ getBlock blockId
  initQuads <- searchBestCutPoint img blockId root
  forM_ initQuads $ \quad -> cutToQuads (level-1) quad
  rememberAvgColors img
  tryMergeAll ToRight tolerance
  tryMergeAll ToTop tolerance
  mergeAllAreas
  paintMergedBlocks tolerance

paintByQuadsSearchBillboard :: Image PixelRGBA8 -> SolverM ()
paintByQuadsSearchBillboard img = paintByQuadsSearchBillboard' img (createBlockId 0)

paintByQuadsSearchBillboard' :: Image PixelRGBA8 -> BlockId -> SolverM ()
paintByQuadsSearchBillboard' img blockId = do
  root <- lift $ getBlock blockId
  initQuads <- searchBestCutPoint img blockId root
  forM_ initQuads $ \quadId -> do
    quad <- lift $ getBlock quadId
    Billboard.solveInside img (quadId, quad)

calcCurrentCost :: Image PixelRGBA8 -> SolverM Integer
calcCurrentCost targetImage = do
  currentImage <- lift $ gets isImage
  cost <- lift $ gets isCost
  return $ cost + (fromIntegral $ imageSimilarity targetImage currentImage)

recursiveHalfs :: Int -> Orientation -> Image PixelRGBA8 -> SolverM ()
recursiveHalfs level dir img = recursiveHalfs' level dir img (createBlockId 0) True

recursiveHalfs' :: Int -> Orientation -> Image PixelRGBA8 -> BlockId -> Bool -> SolverM ()
recursiveHalfs' level dir img blockId isRoot = do
  when isRoot $
    paintAverage blockId
  if level == 0
    then return ()
    else do
      root <- lift $ getBlock blockId
      let middle
            | dir == Vertical = rX root + (rWidth root `div` 2)
            | otherwise = rY root + (rHeight root `div` 2)
      issueMove $ LineCut blockId dir middle
      let subBlock1 = blockId +. 0
          subBlock2 = blockId +. 1
      currentCost <- calcCurrentCost img
      (subCost1, subSolverSt1, subInterpSt1) <- try $ paintAverage subBlock1
      (subCost2, subSolverSt2, subInterpSt2) <- try $ paintAverage subBlock2
      if subCost1 < subCost2
        then do
          lift $ put subInterpSt1
          put subSolverSt1
          recursiveHalfs' (level-1) (anotherOrientation dir) img subBlock1 False
        else do
          lift $ put subInterpSt2
          put subSolverSt2
          recursiveHalfs' (level-1) (anotherOrientation dir) img subBlock2 False
  where
    integralImg = makeIntegralImage img

    try solver = doAndRollback $ do
      solver
      calcCurrentCost img

    paintAverage blockId = do
      block <- lift $ getBlock blockId
      let avgColor = calcAvgColorFromIntegral integralImg block
      issueMove $ SetColor blockId avgColor

recursiveHalfs2 :: Int -> Orientation -> Image PixelRGBA8 -> SolverM ()
recursiveHalfs2 level dir img = recursiveHalfs2' level dir img (createBlockId 0) (createBlockId 1)

binarySearch :: Ord a => V.Vector a -> a -> Int
binarySearch vector needle = search 0 (V.length vector - 1)
  where
    search min max =
      if min+1 >= max
        then min
        else let middle = (min + max) `div` 2
                 midItem = vector V.! middle
             in  if midItem == needle
                   then middle
                   else if needle < midItem
                          then search min middle
                          else search middle max

recursiveHalfs2' :: Int -> Orientation -> Image PixelRGBA8 -> BlockId -> BlockId -> SolverM ()
recursiveHalfs2' level dir img blockId subBlockToPaint = go level dir blockId subBlockToPaint
  where
    go level dir blockId subBlockToPaint = do
      root <- lift $ getBlock blockId
      let commonAvg = calcAvgColor img root
      when (blockId == subBlockToPaint) $ do
        issueMove $ SetColor blockId commonAvg
      if level == 0
        then return ()
        else do
          let (runningDeltasByRows, runningDeltasByColumns) = imagePartDeviations img root commonAvg
              totalByRows = V.last runningDeltasByRows
              totalByColumns = V.last runningDeltasByColumns
              goodRow = binarySearch runningDeltasByRows (totalByRows / 2.0)
              goodColumn = binarySearch runningDeltasByColumns (totalByColumns / 2.0)
              (cutLine, size)
                | dir == Horizontal = (rY root + goodRow, rHeight root)
                | otherwise = (rX root + goodColumn, rWidth root)
              
          let [shape1, shape2] = cutShape root cutLine
          -- trace (printf "Cut: %s %s %s => %s, %s" (show root) (show dir) (show cutLine) (show shape1) (show shape2)) $ return ()
          let subBlockId1 = blockId +. 0
              subBlockId2 = blockId +. 1
          let (smallerSubBlock, largerSubBlockId)
                | cutLine > size `div` 2 = (shape2, subBlockId1)
                | otherwise = (shape1, subBlockId2)
          let smallerAvgColor = calcAvgColor img smallerSubBlock
          issueMove $ SetColor blockId smallerAvgColor
          issueMove $ LineCut blockId dir cutLine
          go (level-1) (anotherOrientation dir) subBlockId1 largerSubBlockId
          go (level-1) (anotherOrientation dir) subBlockId2 largerSubBlockId

    integralImg = makeIntegralImage img

    cutShape :: Shape -> Coordinate -> [Shape]
    cutShape shape line
      | dir == Vertical = cutHorizontalShape shape line
      | otherwise = cutVerticalShape shape line

solveRecursiveAndMerge :: Pixel8 -> Image PixelRGBA8 -> SolverM ()
solveRecursiveAndMerge tolerance img = do
    let root = Rectangle 0 0 (imageWidth img) (imageHeight img)
    quads <- searchBestCutPoint img (createBlockId 0) root
    forM_ quads subdivide
    rememberAvgColors img
    tryMergeAll ToRight tolerance
    tryMergeAll ToTop tolerance
    mergeAllAreas
    paintMergedBlocks tolerance
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
          -- trace (printf "common %f, Q %f, X %f, Y %f" commonRho quadRhos halfsXRhos halfsYRhos) $ return ()
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

