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

module Alt.Solver.Billboard (solve) where

import Codec.Picture.Types
import Control.Parallel.Strategies
import Data.List
import qualified Data.Set as S
import qualified Data.Vector as V

import Alt.AST
import Alt.Evaluator
import Types
import Util

solve :: Image PixelRGBA8 -> Program
solve problem =
  let avgs = avgColorPerColumn problem
      dumbified_avgs =
          S.toList
        $ S.fromList
        $ takeWhile (\l -> S.size (S.fromList l) > 1)
        $ map (flip simplify (V.toList avgs)) [0..]
      programs = parMap rpar produceProgram dumbified_avgs
      results =
        zip
          programs
          (parMap rpar (\program -> evaluateProgram problem program Nothing) programs)
      best = minimumBy (\(_, r1) (_, r2) -> compare (erCost r1) (erCost r2)) results
  in fst best

avgColorPerColumn :: Image PixelRGBA8 -> V.Vector PixelRGBA8
avgColorPerColumn image =
  let width = imageWidth image
      initial = V.replicate width (0, 0, 0, 0)
      sums = pixelFold go initial image
      avgs = V.map (sumsToPixel width) sums
  in avgs
  where
  go :: V.Vector (Integer, Integer, Integer, Integer) -> Int -> Int -> PixelRGBA8 -> V.Vector (Integer, Integer, Integer, Integer)
  go acc x _y (PixelRGBA8 r g b a) =
    let (accR, accG, accB, accA) = acc V.! x
        newValues = (accR + fromIntegral r, accG + fromIntegral g, accB + fromIntegral b, accA + fromIntegral a)
    in acc V.// [(x, newValues)]

sumsToPixel :: Int -> (Integer, Integer, Integer, Integer) -> PixelRGBA8
sumsToPixel width (r, g, b, a) =
  let divide x y = round $ ((fromIntegral x) / (fromIntegral y) :: Double)
  in PixelRGBA8
    (r `divide` width)
    (g `divide` width)
    (b `divide` width)
    (a `divide` width)

produceProgram :: [PixelRGBA8] -> Program
produceProgram colors =
  let (h, t) = span (== head colors) colors
  in go [SetColor (createBlockId 0) (head colors)] 0 (length h) t
  where
  go program _   _ []   = reverse $ dropTrailingMerge program
  go program bId x colorsTail@(color:_) =
    let currentBlockId = createBlockId bId
        action1 = LineCut currentBlockId Vertical x
        leftBlockId = currentBlockId +. 0
        rightBlockId = currentBlockId +. 1
        action2 = SetColor rightBlockId color
        action3 = Merge leftBlockId rightBlockId
        (h, t) = span (== color) colorsTail
    in go (action3:action2:action1:program) (bId+1) (x+length h) t

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
