
module DummySolver where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.List (minimumBy)
import Data.Function (on)
import Codec.Picture.Types

import PNG (readPng, readPngImage, calcAvgColor, subImage)
import Types
import AST
import Interpreter
import Evaluator

type ProgramM a = State Program a

putMove :: Move -> ProgramM ()
putMove m = modify $ \p -> m : p

cutToPixels :: Block -> ProgramM [Block]
cutToPixels block = do
  let shape = blockShape block
      middleX = rX shape + (rWidth shape `div` 2)
      middleY = rY shape + (rHeight shape `div` 2)
  if rWidth shape > 1 && rHeight shape > 1
    then do
         let children = cutPoint block (Point middleX middleY)
         putMove $ PointCut block (Point middleX middleY)
         concat <$> mapM cutToPixels children
    else if rWidth shape > 1
           then do
                let children = cutLine block Vertical middleX
                putMove $ LineCut block Vertical middleX
                concat <$> mapM cutToPixels children
           else if rHeight shape > 1
                  then do
                       let children = cutLine block Horizontal middleY
                       putMove $ LineCut block Horizontal middleY
                       concat <$> mapM cutToPixels children
                  else return [block]

cutToQuads :: Int -> Block -> ProgramM [Block]
cutToQuads 0 block = return [block]
cutToQuads level block = do
  let shape = blockShape block
      middleX = rX shape + (rWidth shape `div` 2)
      middleY = rY shape + (rHeight shape `div` 2)
      children = cutPoint block (Point middleX middleY)
  putMove $ PointCut block (Point middleX middleY)
  concat <$> mapM (cutToQuads (level-1)) children

-- | This implementation suits for 1x1 blocks only
buildMap :: [Block] -> M.Map Point Block
buildMap blocks = M.fromList [(Point (rX (blockShape b)) (rY (blockShape b)), b) | b <- blocks]

findBlock :: M.Map Point Block -> Point -> Block
findBlock blocks point =
  case M.lookup point blocks of
    Nothing -> error $ "Can't find point: " ++ show point
    Just block -> block

drawByPixels :: Block -> [(Point, Color)] -> ProgramM ()
drawByPixels root colors = do
  pixels <- cutToPixels root
  let blockMap = buildMap pixels
  forM_ colors $ \(point, color) -> do
    let block = findBlock blockMap point
    putMove (SetColor block color)

drawPng :: FilePath -> IO Program
drawPng path = do
  (width, height, picture) <- readPng path
  let root = Left $ SimpleBlock (createBlockId 0) (Rectangle 0 0 (width) (height)) white
  let (_, program) = runState (drawByPixels root picture) []
  return $ reverse program

drawPngAvgColor :: FilePath -> IO Program
drawPngAvgColor path = do
  img <- readPngImage path
  let shape = Rectangle 0 0 (imageWidth img) (imageHeight img)
      root = Left $ SimpleBlock (createBlockId 0) shape white
      avgColor = calcAvgColor img shape
      paint = SetColor root avgColor
      program = [paint]
  return program

drawPngAvgQuadsColor :: FilePath -> IO Program
drawPngAvgQuadsColor path = do
  img <- readPngImage path
  let rootShape = Rectangle 0 0 (imageWidth img) (imageHeight img)
      root = Left $ SimpleBlock (createBlockId 0) rootShape white
      middleX = rX rootShape + (rWidth rootShape `div` 2)
      middleY = rY rootShape + (rHeight rootShape `div` 2)
      center = Point middleX middleY
      quads = cutPoint root center
      cut = PointCut root center
      paints = [SetColor quad (calcAvgColor img (blockShape quad)) | quad <- quads]
  return $ cut : paints

drawPngQuadsSearch :: FilePath -> IO Program
drawPngQuadsSearch path = do
  img <- readPngImage path
  let rootShape = Rectangle 0 0 (imageWidth img) (imageHeight img)
      root = Left $ SimpleBlock (createBlockId 0) rootShape white
      checkPoint p =
        let quads = cutPoint root p
            subAvgColors = [calcAvgColor img (blockShape quad) | quad <- quads]
            subRhos = [imagePartDeviation img (blockShape quad) avg | (quad, avg) <- zip quads subAvgColors]
        in  (p, sum subRhos, quads, subAvgColors)
      allPoints = [Point x y | x <- [1, 11 .. imageWidth img - 2], y <- [1, 11 .. imageHeight img - 2]]
      checkResults = map checkPoint allPoints
      (bestPoint, _, bestQuads, avgColors) = minimumBy (compare `on` \(_,rho,_,_) -> rho) checkResults
      cut = PointCut root bestPoint
      paints = zipWith SetColor bestQuads avgColors
  -- print checkResults
  return $ cut : paints

drawPngAverageQuads :: FilePath -> Bool -> Int -> IO Program
drawPngAverageQuads path reset levels = do
  img <- readPngImage path
  let rootShape = Rectangle 0 0 (imageWidth img) (imageHeight img)
      root = Left $ SimpleBlock (createBlockId 0) rootShape white
      (quads, cuts) = runState (cutToQuads levels root) []
      mkCommand quad =
        let color = calcAvgColor img (blockShape quad)
        in  if color == white
              then Nothing
              else Just $ SetColor quad color
      paints = mapMaybe mkCommand quads
      initPaint = if reset then [SetColor root white] else []
  return $ initPaint ++ reverse cuts ++ paints

solveRecursiveP :: Image PixelRGBA8 -> Block -> ProgramM [Block]
solveRecursiveP img block = do
  if rWidth (blockShape block) <= 50 || rHeight (blockShape block) <= 50
    then do
         let color = calcAvgColor img (blockShape block)
         putMove $ SetColor block color
         return [block]
    else do
         let shape = blockShape block
             middleX = rX shape + (rWidth shape `div` 2)
             middleY = rY shape + (rHeight shape `div` 2)
             mid = Point middleX middleY
             children = cutPoint block (Point middleX middleY)
             commonAvgColor = calcAvgColor img shape
             commonRho = imagePartDeviation img shape commonAvgColor
             subAvgColors = [calcAvgColor img (blockShape child) | child <- children]
             subRhos = [imagePartDeviation img (blockShape child) avg | (child, avg) <- zip children subAvgColors]
         if sum subRhos < commonRho
           then do
                putMove $ PointCut block mid
                concat <$> mapM (solveRecursiveP img) children
           else do
                putMove $ SetColor block commonAvgColor
                return [block]

solveRecursive :: FilePath -> IO Program
solveRecursive path = do
  img <- readPngImage path
  let rootShape = Rectangle 0 0 (imageWidth img) (imageHeight img)
      root = Left $ SimpleBlock (createBlockId 0) rootShape white
      (_, program) = runState (solveRecursiveP img root) []
  return $ reverse program

