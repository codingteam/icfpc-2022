{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Json where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Vector as V

import Types

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

instance FromJSON BlockId where
  parseJSON (String s) = 
    return $ BlockId $ reverse $ map (read . T.unpack) $ T.splitOn "." s
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

