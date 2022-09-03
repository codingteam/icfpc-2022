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

-- subImage :: Image PixelRGBA8 -> Shape -> Image PixelRGBA8
-- subImage img shape = crop x y w h img
--   where
--     w = rWidth shape
--     h = rHeight shape
--     x = rX shape
--     y = rY shape

subImage :: Image PixelRGBA8 -> Shape -> Image PixelRGBA8
subImage img shape = Image (rWidth shape) (rHeight shape) subData
   where
    p = 4
    w = imageWidth img
    h = imageHeight img
    w' = rWidth shape
    h' = rHeight shape
    x = rX shape
    y = h - rY shape
    vector = imageData img
    imageRows = V.fromList [VS.convert $ VS.slice (w*p*i) (w*p) vector | i <- [0 .. h-1]]
    rows = V.slice (y - h') h' imageRows
    subRows = V.map (V.slice (p*x) (p*w')) rows
    subData = VS.convert $ V.concat (V.toList subRows)


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

data PixelRGBA = PixelRGBA{ r :: !Integer, g :: !Integer, b :: !Integer, a :: !Integer}

pxAdd :: PixelRGBA -> PixelRGBA -> PixelRGBA
pxAdd p1 p2 = PixelRGBA (r p1 + r p2) (g p1 + g p2) (b p1 + b p2) (a p1 + a p2)
pxSub:: PixelRGBA -> PixelRGBA -> PixelRGBA
pxSub p1 p2 = PixelRGBA (r p1 - r p2) (g p1 - g p2) (b p1 - b p2) (a p1 - a p2)

data IntegralImageRGBA = IntegralImageRGBA{
  width :: Int,
  height :: Int,
  pixels :: V.Vector PixelRGBA
}

makeIntegralImage :: Image PixelRGBA8 -> IntegralImageRGBA
makeIntegralImage src = IntegralImageRGBA (srcWidth + 1) (srcHeight + 1) pixels
  where
    pixels = V.fromList $ concat $ integralRows
    srcWidth = imageWidth src
    srcHeight = imageHeight src
    addRows r1 r2 = map (uncurry pxAdd) $ zip r1 r2
    integralRows = makeIntegral (addRows) zeroRow (map (makeIntegral (pxAdd) zeroPixel ) imgRows)
    zeroPixel = PixelRGBA 0 0 0 0
    zeroRow = [zeroPixel | _i <- [0..srcWidth]]
    imgRows = [getImgRow i | i <- [0..srcHeight - 1]]
    getImgRow j = [px i j | i <- [0..srcWidth - 1]]
      where
        px i j = let PixelRGBA8 r g b a = pixelAt src i j in PixelRGBA (toInteger r) (toInteger g) (toInteger b) (toInteger a)
    makeIntegral add zero xs = [x i | i <- [0..length xs]]
      where
        x 0 = zero
        x i = add (x (i - 1)) (xs !! (i - 1))

getPixel :: IntegralImageRGBA -> Int -> Int -> PixelRGBA
getPixel img x y = pixels img V.! (y * width img + x)

calcAvgColorFromIntegral :: IntegralImageRGBA -> Shape -> Color
calcAvgColorFromIntegral img shape =
    let pxA = getPixel img (rX shape) (rY shape)
        pxB = getPixel img (rX shape + rWidth shape) (rY shape)
        pxC = getPixel img (rX shape) (rY shape + rHeight shape)
        pxD = getPixel img (rX shape + rWidth shape) (rY shape + rHeight shape)
        PixelRGBA sumR sumG sumB sumA = pxSub (pxAdd pxA pxD) (pxAdd pxB pxC)
        size = fromIntegral (rWidth shape * rHeight shape) :: Float
        (avgR, avgG, avgB, avgA) = (fromIntegral sumR / size, fromIntegral sumG / size, fromIntegral sumB / size, fromIntegral sumA / size)
        colorAvg = PixelRGBA8 (round avgR) (round avgG) (round avgB) 255
    in colorAvg

