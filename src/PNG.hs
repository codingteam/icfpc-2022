{-# LANGUAGE TypeFamilies #-}

module PNG where

import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

import Codec.Picture.Types
import Codec.Picture.Png (decodePng)
import Codec.Picture.Extra

import Types(Color, Coordinate, Point(..), Shape (..))

readPng :: FilePath -> IO (Coordinate, Coordinate, [(Point, Color)])
readPng path = do
  pngData <- B.readFile path
  let Right (ImageRGBA8 img) = decodePng pngData
  let pixels = [(Point x y, pixelAt img x (imageHeight img - y - 1)) | x <- [0 .. imageWidth img-1], y <- [0 .. imageHeight img-1]]
  return (imageWidth img, imageHeight img, pixels)

readPngImage :: FilePath -> IO (Image PixelRGBA8)
readPngImage path = do
  pngData <- B.readFile path
  let Right (ImageRGBA8 img) = decodePng pngData
  return img

data PixelRGBAF = PixelRGBAF Float Float Float Float
  deriving (Eq, Show)

pixelPlus :: PixelRGBAF -> PixelRGBA8 -> PixelRGBAF
pixelPlus (PixelRGBAF r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
  PixelRGBAF (r1 + fromIntegral r2) (g1 + fromIntegral g2) (b1 + fromIntegral b2) (a1 + fromIntegral a2)

pixelDiff :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
pixelDiff (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) = PixelRGBA8 (r1-r2) (g1-g2) (b1-b2) (a1-a2)

pixelToFloat :: PixelRGBA8 -> PixelRGBAF
pixelToFloat (PixelRGBA8 r g b a) = PixelRGBAF (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

subImage :: Image PixelRGBA8 -> Shape -> Image PixelRGBA8
subImage img shape = crop x y w h img
  where
    w = rWidth shape
    h = rHeight shape
    x = rX shape
    y = rY shape

-- subImage :: Image PixelRGBA8 -> Shape -> Image PixelRGBA8
-- subImage img shape = Image (rWidth shape) (rHeight shape) subData
--    where
--     p = 4
--     w = imageWidth img
--     h = imageHeight img
--     w' = rWidth shape
--     h' = rHeight shape
--     x = rX shape
--     y = h - rY shape
--     vector = imageData img
--     imageRows = V.fromList [VS.convert $ VS.slice (w*p*i) (w*p) vector | i <- [0 .. h-1]]
--     rows = V.slice (y - h') h' imageRows
--     subRows = V.map (V.slice (p*x) (p*w')) rows
--     subData = VS.convert $ V.concat (V.toList subRows)


calcAvgColor :: Image PixelRGBA8 -> Shape -> Color
calcAvgColor img shape =
  let summate acc x y pixel =
        let y' = imageHeight img - y
        in  if x >= rX shape && x < (rX shape + rWidth shape) && y' >= rY shape && y' < (rY shape + rHeight shape)
              then pixelPlus acc pixel
              else acc
      PixelRGBAF sumR sumG sumB sumA = pixelFold summate (PixelRGBAF 0 0 0 0) img
      -- size = 1
      size = fromIntegral (rWidth shape * rHeight shape) :: Float
      (avgR, avgG, avgB, avgA) = (sumR / size, sumG / size, sumB / size, sumA / size)
      colorAvg = PixelRGBA8 (round avgR) (round avgG) (round avgB) 255
  in colorAvg

