
module Interpreter where

import Control.Monad
import Control.Monad.State

import Types
import AST
import ShapeUtils

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

cutPoint :: Block -> Point -> [Block]
cutPoint b point =
  [Left $ SimpleBlock (blockId b +. i) shape transparent | (i, shape) <- zip [0..] (cutPointShape (blockShape b) point)]

cutLine :: Block -> Orientation -> Coordinate -> [Block]
cutLine block o line =
  [Left $ SimpleBlock (blockId block +. i) shape transparent | (i, shape) <- zip [0..] (cutLineShape (blockShape block) o line)]

