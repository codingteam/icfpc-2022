module PNG where

import qualified Data.ByteString as B

import Codec.Picture.Types
import Codec.Picture.Png (decodePng)

import Types(Color, Coordinate, Point(..))
  
readPng :: FilePath -> IO (Coordinate, Coordinate, [(Point, Color)])
readPng path = do
  pngData <- B.readFile path
  let Right (ImageRGBA8 img) = decodePng pngData
  let pixels = [(Point x y, pixelAt img x y) | x <- [0 .. imageWidth img-1], y <- [0 .. imageHeight img-1]]
  return (imageWidth img, imageHeight img, pixels)
