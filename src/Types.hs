
module Types where

import Data.List (intercalate)
import Codec.Picture.Types (PixelRGBA8)

newtype BlockId = BlockId [Int]
  deriving (Eq)

instance Show BlockId where
  show (BlockId ids) = intercalate "." $ map show ids

type Color = PixelRGBA8

type Coordinate = Int

data Shape = Rectangle {rMinX :: Coordinate, rMinY :: Coordinate, rWidth :: Coordinate, rHeight :: Coordinate}
  deriving (Eq, Show)

data SimpleBlock = SimpleBlock {
    sBlockId :: BlockId
  , blockShape :: Shape
  , blockColor :: Color
  }
  deriving (Eq, Show)

data ComplexBlock = ComplexBlock {
    cBlockId :: BlockId
  , cShape :: Shape
  , cChildren :: ChildBlocks
  }
  deriving (Eq, Show)

type ChildBlocks = [Block]

type Block = Either SimpleBlock ComplexBlock

data Point = Point {pX :: Coordinate, pY :: Coordinate}
  deriving (Eq, Show)

data Orientation = Vertical | Horizontal
  deriving (Eq, Show)

