
module Types (
    BlockId (..), (+.),
    Color, PixelRGBA8 (..), transparent,
    Coordinate,
    Shape (..),
    SimpleBlock (..),
    ComplexBlock (..),
    ChildBlocks,
    Block,
    blockId, blockShape,
    Point (..),
    Orientation (..)
  ) where

import Data.List (intercalate)
import Codec.Picture.Types (PixelRGBA8 (..))
import Text.Printf

newtype BlockId = BlockId [Int]
  deriving (Eq)

(+.) :: BlockId -> Int -> BlockId
(BlockId ids) +. k = BlockId (ids ++ [k])

instance Show BlockId where
  show (BlockId ids) = intercalate "." $ map show ids

type Color = PixelRGBA8

transparent :: Color
transparent = PixelRGBA8 0 0 0 0

type Coordinate = Int

data Shape = Rectangle {rX :: Coordinate, rY :: Coordinate, rWidth :: Coordinate, rHeight :: Coordinate}
  deriving (Eq)

instance Show Shape where
  show r = printf "[(%d,%d), size (%d,%d)]" (rX r) (rY r) (rWidth r) (rHeight r)

data SimpleBlock = SimpleBlock {
    sBlockId :: BlockId
  , sShape :: Shape
  , blockColor :: Color
  }
  deriving (Eq)

instance Show SimpleBlock where
  show b = printf "<S %s %s (%s)>" (show $ sBlockId b) (show $ sShape b) (show $ blockColor b)

data ComplexBlock = ComplexBlock {
    cBlockId :: BlockId
  , cShape :: Shape
  , cChildren :: ChildBlocks
  }
  deriving (Eq)

instance Show ComplexBlock where
  show b = printf "<C %s %s: %s>" (show $ cBlockId b) (show $ cShape b) (show $ cChildren b)

type ChildBlocks = [Block]

type Block = Either SimpleBlock ComplexBlock

blockId :: Block -> BlockId
blockId (Left simple) = sBlockId simple
blockId (Right complex) = cBlockId complex

blockShape :: Block -> Shape
blockShape (Left simple) = sShape simple
blockShape (Right complex) = cShape complex

data Point = Point {pX :: Coordinate, pY :: Coordinate}
  deriving (Eq, Show)

data Orientation = Vertical | Horizontal
  deriving (Eq, Show)
