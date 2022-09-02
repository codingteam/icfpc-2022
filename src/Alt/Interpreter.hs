{-# LANGUAGE RecordWildCards #-}

module Alt.Interpreter where

import Codec.Picture.Types
import Control.Monad
import Control.Monad.State
import Control.Monad.ST (runST)
import qualified Data.Map as M

import Alt.AST
import Types

data InterpreterState = InterpreterState {
    isLastBlockId :: Int
  , isBlocks :: M.Map BlockId Shape
  , isImage :: Image PixelRGBA8
  }

type InterpretM a = State InterpreterState a

-- | Given canvas dimensions, create the initial state.
initialState :: (Coordinate, Coordinate) -> InterpreterState
initialState (width, height) =
  let whitePixel = PixelRGBA8 255 255 255 255
      image = runST $ do
        img <- createMutableImage width height whitePixel
        freezeImage img
  in  InterpreterState {
        isLastBlockId = 0
      , isBlocks = M.singleton (BlockId [0]) (Rectangle 0 0 width height)
      , isImage = image
      }

interpretProgram :: Program -> InterpretM ()
interpretProgram p = forM_ p interpretMove

interpretMove :: Move -> InterpretM ()
interpretMove (PointCut blockId (Point x y)) = modify' $ \is ->
  case blockId `M.lookup` (isBlocks is) of
    Nothing -> is
    Just parent ->
      let dx = x - rX parent
          dy = y - rY parent
          bottomLeft = Rectangle (rX parent) (rY parent) dx dy
          bottomRight = Rectangle (rX parent + dx) (rY parent) (rWidth parent - dx) dy
          topRight = Rectangle (rX parent + dx) (rY parent + dy) (rWidth parent - dx) (rHeight parent - dy)
          topLeft = Rectangle (rX parent) (rY parent + dy) dx (rHeight parent - dy)

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
    Just parent ->
      let dy = y - rY parent
          top = Rectangle (rX parent) (rY parent + dy) (rWidth parent) (rHeight parent - dy)
          bottom = Rectangle (rX parent) (rY parent) (rWidth parent) dy
          blocks' =
              M.insert (blockId +. 0) bottom
            $ M.insert (blockId +. 1) top
            $ M.delete blockId
            $ isBlocks is
      in is { isBlocks = blocks' }
interpretMove (LineCut blockId Vertical x) = modify' $ \is ->
  case blockId `M.lookup` (isBlocks is) of
    Nothing -> is
    Just parent ->
      let dx = x - rX parent
          left = Rectangle (rX parent) (rY parent) dx (rHeight parent)
          right = Rectangle (rX parent + dx) (rY parent) (rWidth parent - dx) (rHeight parent)
          blocks' =
              M.insert (blockId +. 0) left
            $ M.insert (blockId +. 1) right
            $ M.delete blockId
            $ isBlocks is
      in is { isBlocks = blocks' }
interpretMove (SetColor blockId color) = modify' $ \is ->
  case blockId `M.lookup` (isBlocks is) of
    Nothing -> is
    Just (Rectangle { .. }) ->
      let image = isImage is
          image' = runST $ do
            img <- thawImage image

            -- We have (0, 0) at bottom left, Juicy Pixels has it at top left
            let h = mutableImageHeight img
            forM_ [(h - rY - rHeight) .. (h - rY - 1)] $ \y -> do
              forM_ [rX .. (rX + rWidth - 1)] $ \x -> do
                writePixel img x y color

            freezeImage img
      in  is { isImage = image' }
interpretMove _ = undefined
