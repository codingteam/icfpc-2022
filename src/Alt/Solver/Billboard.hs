{-# LANGUAGE BangPatterns #-}

-- The larger the area, the cheaper it is to cut it into pieces. In limit, this
-- means we should always cut the *smallest* possible piece from the *largest*
-- possible area. The billboard solver is a simple implementation of this idea:
-- it cuts the image into vertical "stripes", and colours each one with an
-- average colour of its pixels.
--
-- As an optimization, adjacent stripes of the same colour are all filled in
-- one go, i.e. the solver skips the cutting step as long as the current column
-- has the same colour as the next one.

module Alt.Solver.Billboard (solve, solveInside) where

import Codec.Picture.Types
import Control.Monad.State
import Control.Parallel.Strategies
import Data.List
import qualified Data.Set as S
import qualified Data.Vector as V

import Alt.AST
import Alt.Evaluator
import Alt.Interpreter
import Alt.SolverM
import PNG (subImage)
import Types
import Util

solve :: Image PixelRGBA8 -> SolverM ()
solve problem =
  let rootBlockShape = Rectangle 0 0 (imageWidth problem) (imageHeight problem)
      rootBlockId = createBlockId 0
  in solveInside problem (rootBlockId, rootBlockShape)

-- | Solve a subset of the problem bounded by the given block.
solveInside :: Image PixelRGBA8 -> (BlockId, Shape) -> SolverM ()
solveInside problem (bId, shape) = do
  let subproblem = subImage problem shape
  let (columnAvgs, rowAvgs) = avgColorPerColumnAndRow subproblem
  let dumbifiedColumnAvgs = uniqueSimplifications columnAvgs
  let dumbifiedRowAvgs = uniqueSimplifications rowAvgs
  lastBlockId <- lift $ gets isLastBlockId
  let programs =
       (parMap rpar (produceProgram bId (lastBlockId+1) Vertical) dumbifiedColumnAvgs)
       ++ (parMap rpar (produceProgram bId (lastBlockId+1) Horizontal) dumbifiedRowAvgs)
  let results =
        zip
          programs
          (parMap rpar (\program -> evaluateProgram problem program Nothing) programs)
  let best = minimumBy (\(_, r1) (_, r2) -> compare (erCost r1) (erCost r2)) results
  mapM_ issueMove $ fst best

avgColorPerColumnAndRow :: Image PixelRGBA8 -> (V.Vector PixelRGBA8, V.Vector PixelRGBA8)
avgColorPerColumnAndRow image =
  let width = imageWidth image
      height = imageHeight image
      initial = ( V.replicate width (0, 0, 0, 0), V.replicate height (0, 0, 0, 0) )
      (columnSums, rowSums) = pixelFold go initial image
      columnAvgs = V.map (sumsToPixel width) columnSums
      rowAvgs = V.map (sumsToPixel height) rowSums
  in (columnAvgs, rowAvgs)
  where
  go
    :: ( V.Vector (Integer, Integer, Integer, Integer), V.Vector (Integer, Integer, Integer, Integer) )
    -> Int
    -> Int
    -> PixelRGBA8
    -> ( V.Vector (Integer, Integer, Integer, Integer), V.Vector (Integer, Integer, Integer, Integer) )
  go (colAcc, rowAcc) x y (PixelRGBA8 r g b a) =
    let (colAccR, colAccG, colAccB, colAccA) = colAcc V.! x
        newColAcc = (colAccR + fromIntegral r, colAccG + fromIntegral g, colAccB + fromIntegral b, colAccA + fromIntegral a)
        (rowAccR, rowAccG, rowAccB, rowAccA) = rowAcc V.! y
        newRowAcc = (rowAccR + fromIntegral r, rowAccG + fromIntegral g, rowAccB + fromIntegral b, rowAccA + fromIntegral a)
    in (colAcc V.// [(x, newColAcc)], rowAcc V.// [(y, newRowAcc)])

sumsToPixel :: Int -> (Integer, Integer, Integer, Integer) -> PixelRGBA8
sumsToPixel width (r, g, b, a) =
  let divide x y = round $ ((fromIntegral x) / (fromIntegral y) :: Double)
  in PixelRGBA8
    (r `divide` width)
    (g `divide` width)
    (b `divide` width)
    (a `divide` width)

produceProgram :: BlockId -> Int -> Orientation -> [PixelRGBA8] -> Program
produceProgram currentBlockId nextBlockIdNumber orientation colors =
  let (h, t) = span (== head colors) colors
  in go [SetColor currentBlockId (head colors)] currentBlockId nextBlockIdNumber (length h) t
  where
  go program _      _      _ []   = reverse $ dropTrailingMerge program
  go program curId nextIdNo x colorsTail@(color:_) =
    let action1 = LineCut curId orientation x
        leftBlockId = curId +. 0
        rightBlockId = curId +. 1
        action2 = SetColor rightBlockId color
        action3 = Merge leftBlockId rightBlockId
        (h, t) = span (== color) colorsTail
    in go (action3:action2:action1:program) (createBlockId nextIdNo) (nextIdNo+1) (x+length h) t

  dropTrailingMerge :: Program -> Program
  dropTrailingMerge (Merge _ _ : p) = p
  dropTrailingMerge p = p

simplify :: Int -> [PixelRGBA8] -> [PixelRGBA8]
simplify _ [] = []
simplify color_diff_tolerance colors@(x:_) =
  let (h, t) = span (\c -> colorDiff x c <= color_diff_tolerance) colors
      avg = averageColor h
      newHead = replicate (length h) avg
      newTail = simplify color_diff_tolerance t
  in newHead ++ newTail

uniqueSimplifications :: V.Vector PixelRGBA8 -> [[PixelRGBA8]]
uniqueSimplifications avgs =
    S.toList
  $ S.fromList
  $ takeWhile (\l -> S.size (S.fromList l) > 1)
  $ map (flip simplify (V.toList avgs)) [0..]

colorDiff :: PixelRGBA8 -> PixelRGBA8 -> Int
colorDiff (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
    jsRound
  $ sqrt
  $ sum
  $ map (\x -> x*x)
  $ zipWith (-)
      (map fromIntegral [r1, g1, b1, a1])
      (map fromIntegral [r2, g2, b2, a2])

averageColor :: [PixelRGBA8] -> PixelRGBA8
averageColor [] = error "averageColor called on empty list!"
averageColor colors =
  let sums = foldl' go (0, 0, 0, 0) colors
  in sumsToPixel (length colors) sums
  where
  go :: (Integer, Integer, Integer, Integer) -> PixelRGBA8 -> (Integer, Integer, Integer, Integer)
  go (accR, accG, accB, accA) (PixelRGBA8 r g b a) =
    (accR + fromIntegral r, accG + fromIntegral g, accB + fromIntegral b, accA + fromIntegral a)
