module Alt.Types (
    BlockId (..), (+.),
    Color, PixelRGBA8 (..), transparent, white,
    Coordinate,
    Shape (..),
    Block (..),
    Point (..),
    Orientation (..)
  ) where

import Types (
  BlockId (..), (+.),
  Color, PixelRGBA8 (..), transparent, white,
  Coordinate,
  Shape (..),
  Point (..),
  Orientation (..))

data Block = Block {
    bShape :: Shape
  , bColor :: Color
  }
  deriving (Eq, Show)
