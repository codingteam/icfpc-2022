{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Json where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Debug.Trace

import Types
import PNG

data Configuration = Configuration {
      cWidth :: Coordinate
    , cHeight :: Coordinate
    , cBlocks :: [BlockJson]
  }
  deriving (Show)

data BlockJson = BlockJson {
      bjId :: BlockId
    , bjBottomLeft :: Point
    , bjTopRight :: Point
    , bjColor :: Color
  }
  deriving (Show)

emptyConfiguration :: (Coordinate,Coordinate) -> Configuration
emptyConfiguration (width, height) = Configuration width height [block]
  where
    block = BlockJson {
                bjId = createBlockId 0
              , bjBottomLeft = Point 0 0
              , bjTopRight = Point width height
              , bjColor = white
            }

instance FromJSON BlockId where
  parseJSON (String s) =
    return $ BlockId $ VU.fromList $ map (read . T.unpack) $ T.splitOn "." s
  parseJSON invalid = typeMismatch "BlockId" invalid

instance FromJSON Point where
  parseJSON (Array a) = do
    lst <- forM (V.toList a) parseJSON
    return $ Point (lst !! 0) (lst !! 1)
  parseJSON invalid = typeMismatch "Point" invalid

instance FromJSON Color where
  parseJSON (Array a) = do
    lst <- forM (V.toList a) parseJSON
    return $ PixelRGBA8 (lst !! 0) (lst !! 1) (lst !! 2) (lst !! 3)
  parseJSON invalid = typeMismatch "Color" invalid

instance FromJSON BlockJson where
  parseJSON (Object v) = BlockJson
    <$> v .: "blockId"
    <*> v .: "bottomLeft"
    <*> v .: "topRight"
    <*> v .: "color"
  parseJSON invalid = typeMismatch "Block" invalid

instance FromJSON Configuration where
  parseJSON (Object v) = Configuration
    <$> v .: "width"
    <*> v .: "height"
    <*> v .: "blocks"
  parseJSON invalid = typeMismatch "Configuration" invalid

parseConfig :: FilePath -> IO Configuration
parseConfig path = do
  r <- eitherDecodeFileStrict' path
  case r of
    Left err -> fail err
    Right cfg -> return cfg

containsPoint :: BlockJson -> Point -> Bool
containsPoint block (Point x y) =
  (x >= pX (bjBottomLeft block)) && (x <= pX (bjTopRight block)) &&
    (y >= pY (bjBottomLeft block)) && (y <= pY (bjTopRight block))

getColorAt :: Configuration -> Point -> Color
getColorAt cfg point =
  let goodBlocks = filter (`containsPoint` point) (cBlocks cfg)
  in  if null goodBlocks
        then error $ "Can't find point in configuration: " ++ show point
        else bjColor $ head goodBlocks 

calcAvgColorFromConfig :: Configuration -> Shape -> Color
calcAvgColorFromConfig cfg shape =
  let points = [Point x y | x <- [rX shape .. rX shape + rWidth shape-1], y <- [rY shape .. rY shape + rHeight shape-1]]
      colors = map (getColorAt cfg) points
      PixelRGBAF sumR sumG sumB sumA = foldr (flip pixelPlus) (PixelRGBAF 0 0 0 0) colors
      size = fromIntegral (rWidth shape * rHeight shape) :: Float
      (avgR, avgG, avgB, avgA) = (sumR / size, sumG / size, sumB / size, sumA / size)
      colorAvg = PixelRGBA8 (round avgR) (round avgG) (round avgB) (round avgA)
  in colorAvg

getBlocks :: Configuration -> [(BlockId, Shape)]
getBlocks cfg = map parse (cBlocks cfg)
  where
    parse block = (bjId block, shape block)
    shape block = Rectangle (pX $ bjBottomLeft block) (pY $ bjBottomLeft block)
                      (pX (bjTopRight block) - pX (bjBottomLeft block))
                      (pY (bjTopRight block) - pY (bjBottomLeft block))

getColoredBlocks :: Configuration -> [(BlockId, Shape, Color)]
getColoredBlocks cfg = map parse (cBlocks cfg)
  where
    parse block = (bjId block, shape block, bjColor block)
    shape block = Rectangle (pX $ bjBottomLeft block) (pY $ bjBottomLeft block)
                      (pX (bjTopRight block) - pX (bjBottomLeft block))
                      (pY (bjTopRight block) - pY (bjBottomLeft block))

