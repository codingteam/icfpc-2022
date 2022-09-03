
module Evaluator where

import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import Codec.Picture.Types

import Types
import Util
import PNG (pixelToFloat, pixelDiff, PixelRGBAF (..))

type Cost = Int

decomposeImg :: Image PixelRGBA8 -> (V.Vector Pixel8, V.Vector Pixel8, V.Vector Pixel8, V.Vector Pixel8)
decomposeImg img =
  (
    VS.convert $ imageData $ extractComponent PlaneRed img,
    VS.convert $ imageData $ extractComponent PlaneGreen img,
    VS.convert $ imageData $ extractComponent PlaneBlue img,
    VS.convert $ imageData $ extractComponent PlaneAlpha img
  )

-- imageToPixelVector :: Image PixelRGBA8 -> V.Vector PixelRGBA8
-- imageToPixelVector img =
--   let (red, green, blue, alpha) = decomposeImg img
--   in  V.zipWith4 PixelRGBA8 red green blue alpha

imageToVector :: Image PixelRGBA8 -> V.Vector (V.Vector Pixel8)
imageToVector img =
  let (red, green, blue, alpha) = decomposeImg img
  in  V.zipWith4 (\r g b a -> V.fromList [r,g,b,a]) red green blue alpha

imageToVector' :: Image PixelRGBA8 -> V.Vector PixelRGBA8
imageToVector' img =
  let (red, green, blue, alpha) = decomposeImg img
  in  V.zipWith4 PixelRGBA8 red green blue alpha

imageToVectorF :: Image PixelRGBA8 -> V.Vector (V.Vector Double)
imageToVectorF img =
  let (red, green, blue, alpha) = decomposeImg img
      [redF, greenF, blueF, alphaF] = map (V.map fromIntegral) [red, green, blue, alpha]
  in  V.zipWith4 (\r g b a -> V.fromList [r,g,b,a]) redF greenF blueF alphaF

sqr :: Double -> Double
sqr x = x*x

imageSimilarity :: Image PixelRGBA8 -> Image PixelRGBA8 -> Cost
imageSimilarity img1 img2 =
  let vector1 = imageToVectorF img1
      vector2 = imageToVectorF img2
      diff = V.zipWith (V.zipWith (-)) vector1 vector2
      pixelDistances = V.map (sqrt . V.sum . V.map sqr) diff
      sumDistance = V.sum pixelDistances
      alpha = 0.005
  in  jsRound $ alpha * sumDistance

pixelNorm :: PixelRGBAF -> Float
pixelNorm (PixelRGBAF r g b a) = sqrt $ r*r + g*g + b*b + a*a

norm :: Float -> Float -> Float -> Float -> Float
norm r g b a = sqrt $ r*r + g*g + b*b + a*a

imageDeviation :: Image PixelRGBA8 -> Color -> Float
imageDeviation img target =
  let allPixels = [pixelAt img x y | x <- [0 .. imageWidth img - 1], y <- [0 .. imageHeight img - 1]]
      diffs = map (pixelDiff target) allPixels
      pixelDistances = map (pixelNorm . pixelToFloat) diffs
      sumDistance = sum pixelDistances
  in sumDistance

imagePartDeviation :: Image PixelRGBA8 -> Shape -> Color -> Float
imagePartDeviation img shape target =
  let x0 = rX shape
      y0 = rY shape
      w = rWidth shape
      h = rHeight shape
      allPixels = [pixelAt img x y | x <- [x0 .. x0+w-1], y <- [y0 .. y0+h-1]]
      diffs = map (pixelDiff target) allPixels
      pixelDistances = map (pixelNorm . pixelToFloat) diffs
      sumDistance = sum pixelDistances
  in sumDistance

