module PNG where

import qualified Data.ByteString as B

import Codec.Picture.Types
import Codec.Picture.Png (decodePng)

import Types(Color, Coordinate, Point(..), Shape (..))
  
readPng :: FilePath -> IO (Coordinate, Coordinate, [(Point, Color)])
readPng path = do
  pngData <- B.readFile path
  let Right (ImageRGBA8 img) = decodePng pngData
  let pixels = [(Point x y, pixelAt img x y) | x <- [0 .. imageWidth img-1], y <- [0 .. imageHeight img-1]]
  return (imageWidth img, imageHeight img, pixels)

readPngImage :: FilePath -> IO (Image PixelRGBA8)
readPngImage path = do
  pngData <- B.readFile path
  let Right (ImageRGBA8 img) = decodePng pngData
  return img

data PixelRGBAF = PixelRGBAF Float Float Float Float
  deriving (Eq, Show)

pixelPlus :: PixelRGBAF -> PixelRGBA16 -> PixelRGBAF
pixelPlus (PixelRGBAF r1 g1 b1 a1) (PixelRGBA16 r2 g2 b2 a2) =
  PixelRGBAF (r1 + fromIntegral r2) (g1 + fromIntegral g2) (b1 + fromIntegral b2) (a1 + fromIntegral a2)

calcAvgColor :: Image PixelRGBA8 -> Shape -> Color
calcAvgColor img shape =
  let img16 = promoteImage img :: Image PixelRGBA16
      summate acc x y pixel
        | x >= rX shape && x < (rX shape + rWidth shape) && y >= rY shape && y < (rY shape + rHeight shape) = pixelPlus acc pixel
        | otherwise = acc
      PixelRGBAF sumR sumG sumB sumA = pixelFold summate (PixelRGBAF 0 0 0 0) img16
      -- size = 1
      size = fromIntegral (imageWidth img * imageHeight img) :: Float
      (avgR, avgG, avgB, avgA) = (sumR / size, sumG / size, sumB / size, sumA / size)
      demote p = round (p / 256)
      colorAvg = PixelRGBA8 (demote avgR) (demote avgG) (demote avgB) (demote avgA)
  in colorAvg

