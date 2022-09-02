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
interpretMove (SetColor blockId color) = modify' $ \is ->
  case blockId `M.lookup` (isBlocks is) of
    Nothing -> is
    Just block ->
      let newBlock = block { bColor = color }
          blocks' = M.insert blockId newBlock (isBlocks is)
      in is { isBlocks = blocks' }
interpretMove _ = undefined
