module Alt.Interpreter where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

import Alt.AST
import Alt.Types

data InterpreterState = InterpreterState {
    isLastBlockId :: Int
  , isBlocks :: M.Map BlockId Block
  , isContains :: M.Map BlockId (S.Set BlockId)
  }
  deriving (Show)

type InterpretM a = State InterpreterState a

-- | Given canvas dimensions, create the initial state.
initialState :: (Coordinate, Coordinate) -> InterpreterState
initialState (width, height) =
  let rootBlock =
        Block {
          bShape = Rectangle 0 0 width height
        , bColor = white
        }
  in  InterpreterState {
        isLastBlockId = 0
      , isBlocks = M.singleton (BlockId [0]) rootBlock
      , isContains = M.empty
      }

interpretProgram :: Program -> InterpretM ()
interpretProgram p = forM_ p interpretMove

interpretMove :: Move -> InterpretM ()
interpretMove (PointCut blockId (Point x y)) = modify' $ \is ->
  case blockId `M.lookup` (isBlocks is) of
    Nothing -> is
    Just block ->
      let parent = bShape block
          dx = x - rX parent
          dy = y - rY parent
          bottomLeft = block { bShape = Rectangle (rX parent) (rY parent) dx dy }
          bottomRight = block { bShape = Rectangle (rX parent + dx) (rY parent) (rWidth parent - dx) dy }
          topRight = block { bShape = Rectangle (rX parent + dx) (rY parent + dy) (rWidth parent - dx) (rHeight parent - dy) }
          topLeft = block { bShape = Rectangle (rX parent) (rY parent + dy) dx (rHeight parent - dy) }

          blocks' =
              M.insert (blockId +. 0) bottomLeft
            $ M.insert (blockId +. 1) bottomRight
            $ M.insert (blockId +. 2) topRight
            $ M.insert (blockId +. 3) topLeft
            $ M.delete blockId
            $ isBlocks is
      in is { isBlocks = blocks' }
interpretMove (LineCut blockId Horizontal y) = modify' $ \is ->
  case blockId `M.lookup` (isBlocks is) of
    Nothing -> is
    Just block ->
      let parent = bShape block
          dy = y - rY parent
          top = block { bShape = Rectangle (rX parent) (rY parent + dy) (rWidth parent) (rHeight parent - dy) }
          bottom = block { bShape = Rectangle (rX parent) (rY parent) (rWidth parent) dy }
          blocks' =
              M.insert (blockId +. 0) bottom
            $ M.insert (blockId +. 1) top
            $ M.delete blockId
            $ isBlocks is
      in is { isBlocks = blocks' }
interpretMove (LineCut blockId Vertical x) = modify' $ \is ->
  case blockId `M.lookup` (isBlocks is) of
    Nothing -> is
    Just block ->
      let parent = bShape block
          dx = x - rX parent
          left = block { bShape = Rectangle (rX parent) (rY parent) dx (rHeight parent) }
          right = block { bShape = Rectangle (rX parent + dx) (rY parent) (rWidth parent - dx) (rHeight parent) }
          blocks' =
              M.insert (blockId +. 0) left
            $ M.insert (blockId +. 1) right
            $ M.delete blockId
            $ isBlocks is
      in is { isBlocks = blocks' }
interpretMove (SetColor blockId color) = modify' $ \is ->
  case blockId `M.lookup` (isBlocks is) of
    Nothing -> is
    Just block ->
      let newBlock = block { bColor = color }
          blocks' = M.insert blockId newBlock (isBlocks is)
      in is { isBlocks = blocks' }
interpretMove _ = undefined
