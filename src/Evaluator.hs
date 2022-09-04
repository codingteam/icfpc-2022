
module Evaluator where

import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import Codec.Picture.Types

import Types
import Util
import PNG (pixelToFloat, pixelDiff, PixelRGBAF (..), subImage)
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

imageDeviation :: Image PixelRGBA8 -> Color -> Float
imageDeviation img target@(PixelRGBA8 tR tG tB tA) =
  let vector = VS.toList $ imageData img
      w = imageWidth img
      h = imageHeight img
      p = 4
      targetV = concat $ replicate (w*h) [tR, tG, tB, tA]
      diffs = zipWith (-) vector targetV
      sqrs = VS.fromList $ map (\x -> fromIntegral (x*x)) diffs
      calcNorm v = sqrt (VS.sum v)
      pixelDistances = [calcNorm (VS.slice (p*i) p sqrs) | i <- [0 .. w*h-1]]
      sumDistance = sum pixelDistances
  in sumDistance

imagePartDeviation :: Image PixelRGBA8 -> Shape -> Color -> Float
imagePartDeviation img shape target = imageDeviation (subImage img shape) target

