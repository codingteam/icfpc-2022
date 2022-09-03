{-# LANGUAGE TypeFamilies #-}

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

pixelPlus :: PixelRGBAF -> PixelRGBA8 -> PixelRGBAF
pixelPlus (PixelRGBAF r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
  PixelRGBAF (r1 + fromIntegral r2) (g1 + fromIntegral g2) (b1 + fromIntegral b2) (a1 + fromIntegral a2)

pixelToFloat :: PixelRGBA8 -> PixelRGBAF
pixelToFloat (PixelRGBA8 r g b a) = PixelRGBAF (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

calcAvgColor :: Image PixelRGBA8 -> Shape -> Color
calcAvgColor img shape =
  let summate acc x y pixel
        | x >= rX shape && x < (rX shape + rWidth shape) && y >= rY shape && y < (rY shape + rHeight shape) = pixelPlus acc pixel
        | otherwise = acc
      PixelRGBAF sumR sumG sumB sumA = pixelFold summate (PixelRGBAF 0 0 0 0) img
      -- size = 1
      size = fromIntegral (rWidth shape * rHeight shape) :: Float
      (avgR, avgG, avgB, avgA) = (sumR / size, sumG / size, sumB / size, sumA / size)
      colorAvg = PixelRGBA8 (round avgR) (round avgG) (round avgB) 255
  in colorAvg

