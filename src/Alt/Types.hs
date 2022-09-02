module Alt.Types (
    BlockId (..), (+.),
    Color, transparent, white,
    Coordinate,
    Shape (..),
    Block (..),
    Point (..),
    Orientation (..)
  ) where

import Types (
  BlockId (..), (+.),
  Color, transparent, white,
  Coordinate,
  Shape (..),
  Point (..),
  Orientation (..))

data Block = Block {
    bShape :: Shape
  , bColor :: Color
  }
  deriving (Eq, Show)
