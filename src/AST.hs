
module AST where

import Types

data Move =
    PointCut Block Point
  | LineCut Block Orientation Coordinate
  | SetColor Block Color
  | Swap Block Block
  | Merge Block Block
  deriving (Eq, Show)

