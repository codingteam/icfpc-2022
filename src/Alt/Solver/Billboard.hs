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
import qualified Data.Vector as V

import Alt.AST
import Types

solve :: Image PixelRGBA8 -> Program
solve problem =
  let avgs = avgColorPerColumn problem
  in produceProgram (V.toList avgs)

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
