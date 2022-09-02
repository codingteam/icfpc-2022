
module Interpreter where

import Control.Monad
import Control.Monad.State

import Types
import AST

data InterpreterState = InterpreterState {
    isLastBlockId :: Int
  , isRootBlock :: Block
  }
  deriving (Eq, Show)

type InterpretM a = State InterpreterState a

interpretProgram :: Program -> InterpretM ()
interpretProgram p = forM_ p interpretMove

interpretMove :: Move -> InterpretM ()
interpretMove = undefined

cutPointShape :: Shape -> Point -> [Shape]
cutPointShape parent (Point x y) =
  let dx = x - rX parent
      dy = y - rY parent
      shape0 = Rectangle (rX parent) (rY parent) dx dy
      shape1 = Rectangle (rX parent + dx) (rY parent) (rWidth parent - dx) dy
      shape2 = Rectangle (rX parent) (rY parent + dy) dx (rHeight parent - dy)
      shape3 = Rectangle (rX parent + dx) (rY parent + dy) (rWidth parent - dx) (rHeight parent - dy)
  in  [shape0, shape1, shape2, shape3]

cutPoint :: Block -> Point -> [Block]
cutPoint b point =
  [Left $ SimpleBlock (blockId b +. i) shape transparent | (i, shape) <- zip [0..] (cutPointShape (blockShape b) point)]

cutHorizontalShape :: Shape -> Coordinate -> [Shape]
cutHorizontalShape parent y =
  let dy = y - rY parent
      shape0 = Rectangle (rX parent) (rY parent) (rWidth parent) dy
      shape1 = Rectangle (rX parent) (rY parent + dy) (rWidth parent) (rHeight parent - dy)
  in  [shape0, shape1]

cutVerticalShape :: Shape -> Coordinate -> [Shape]
cutVerticalShape parent x =
  let dx = x - rX parent
      shape0 = Rectangle (rX parent) (rY parent) dx (rHeight parent)
      shape1 = Rectangle (rX parent + dx) (rY parent) (rWidth parent - dx) (rHeight parent)
  in [shape0, shape1]

cutLineShape :: Shape -> Orientation -> Coordinate -> [Shape]
cutLineShape parent Horizontal y = cutHorizontalShape parent y
cutLineShape parent Vertical x = cutVerticalShape parent x

cutLine :: Block -> Orientation -> Coordinate -> [Block]
cutLine block o line =
  [Left $ SimpleBlock (blockId block +. i) shape transparent | (i, shape) <- zip [0..] (cutLineShape (blockShape block) o line)]

