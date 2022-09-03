
module Evaluator where

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Generic as VG
import qualified Data.Vector as V
import Codec.Picture.Types
import Foreign.Storable

import Types
import PNG

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

sqr :: Float -> Float
sqr x = x*x

imageSimilarity :: Image PixelRGBA8 -> Image PixelRGBA8 -> Cost
imageSimilarity img1 img2 =
  let vector1 = imageToVector img1
      vector2 = imageToVector img2
      diff = V.zipWith (V.zipWith (-)) vector1 vector2
      diffF = V.map (V.map fromIntegral) diff :: V.Vector (V.Vector Float)
      pixelDistances = V.map (sqrt . V.sum . V.map sqr) diffF
      sumDistance = V.sum pixelDistances
      alpha = 0.005
  in  round $ alpha * sumDistance

