module Alt.AST where

import Types

type Program = [Move]

data Move =
    PointCut BlockId Point
  | LineCut BlockId Orientation Coordinate
  | SetColor BlockId Color
  | Swap BlockId BlockId
  | Merge BlockId BlockId
  deriving (Eq, Show)

