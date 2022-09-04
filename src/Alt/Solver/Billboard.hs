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

solve :: Image PixelRGBA8 -> Int -> Program
solve problem max_color_diff_tolerance =
  let avgs = avgColorPerColumn problem
      dumbified_avgs =
          S.toList
        $ S.fromList
        $ map (flip simplify (V.toList avgs)) [0 .. max_color_diff_tolerance]
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
produceProgram = go [] (createBlockId 0) 1
  where
  go program _   _ []   = reverse $ program
  go program bId x [color] =
    let action = SetColor bId color
        nextBlockId = bId +. 1
    in go (action:program) nextBlockId (x+1) []
  go program bId x (color:nextColor:avgs)
    | color == nextColor = go program bId (x+1) (nextColor:avgs)
    | otherwise =
        let action1 = SetColor bId color
            action2 = LineCut bId Vertical x
            nextBlockId = bId +. 1
        in go (action2:action1:program) nextBlockId (x+1) (nextColor:avgs)

simplify :: Int -> [PixelRGBA8] -> [PixelRGBA8]
simplify color_diff_tolerance = go
  where
  go [] = []
  go [c] = [c]
  go (c1:c2:avgs)
    | colorDiff c1 c2 < color_diff_tolerance = c1 : go (c1:avgs)
    | otherwise = c1 : go (c2:avgs)

colorDiff :: PixelRGBA8 -> PixelRGBA8 -> Int
colorDiff (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
    jsRound
  $ sqrt
  $ sum
  $ map (\x -> x*x)
  $ zipWith (-)
      (map fromIntegral [r1, g1, b1, a1])
      (map fromIntegral [r2, g2, b2, a2])
