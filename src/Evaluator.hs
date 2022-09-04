
module Evaluator where

import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import Codec.Picture.Types

import Types
import Util
import PNG (pixelToFloat, PixelRGBAF (..))
import Data.List (foldl')

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

pixelZipFold :: (Pixel pixel)
          => (acc -> Int -> Int -> pixel -> pixel -> acc)
          -> acc -> Image pixel -> Image pixel -> acc
pixelZipFold
      f initialAccumulator
      img1@(Image { imageWidth = w, imageHeight = h })
      img2@(Image { imageWidth = w2, imageHeight = h2 }) =
    -- TODO: assert w == w2 && h == h2
    foldl' columnFold initialAccumulator [0 .. h - 1]
      where
        pixelZipFolder y acc x = f acc x y (pixelAt img1 x y) (pixelAt img2 x y)
        columnFold lineAcc y = foldl' (pixelZipFolder y) lineAcc [0 .. w - 1]

sqr :: (Num a) => a -> a
sqr x = x*x

pixelSimilarity :: PixelRGBA8 -> PixelRGBA8 -> Float
pixelSimilarity p1 p2 =
  let PixelRGBAF r1 g1 b1 a1 = pixelToFloat p1
      PixelRGBAF r2 g2 b2 a2 = pixelToFloat p2
  in sqrt $ sqr (r1 - r2) + sqr (g1 - g2) + sqr (b1 - b2) + sqr (a1 - a2)

imageSimilarity :: Image PixelRGBA8 -> Image PixelRGBA8 -> Cost
imageSimilarity img1 img2 = jsRound $ realToFrac $ 0.005 * pixelZipFold f 0 img1 img2
  where f acc _ _ p1 p2 = acc + pixelSimilarity p1 p2

pixelNorm :: PixelRGBAF -> Float
pixelNorm (PixelRGBAF r g b a) = sqrt $ r*r + g*g + b*b + a*a

norm :: Float -> Float -> Float -> Float -> Float
norm r g b a = sqrt $ r*r + g*g + b*b + a*a

imagePartDeviation :: Image PixelRGBA8 -> Shape -> Color -> Float
imagePartDeviation img shape target =
  foldl' columnFold 0 [imageHeight img - rY shape - rHeight shape .. imageHeight img - rY shape - 1]
  where
    pixelFolder y acc x = acc + pixelSimilarity (pixelAt img x y) target
    columnFold lineAcc y = foldl' (pixelFolder y) lineAcc [rX shape .. rX shape + rWidth shape - 1]
