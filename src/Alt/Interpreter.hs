{-# LANGUAGE RecordWildCards #-}

module Alt.Interpreter where

import Codec.Picture.Types
import Control.Monad
import Control.Monad.State
import Control.Monad.ST (ST, runST)
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
interpretMove (Swap blockId1 blockId2) = modify' $ \is ->
  let blocks = isBlocks is
  in case (blockId1 `M.lookup` blocks, blockId2 `M.lookup` blocks) of
    (Just shape1, Just shape2) ->
      let image = isImage is
          image' = runST $ do
            img <- thawImage image

            copyShape image shape1 img shape2
            copyShape image shape2 img shape1

            freezeImage img
          blocks' =
              M.insert blockId1 shape2
            $ M.insert blockId2 shape1
            $ isBlocks is
      in is { isImage = image', isBlocks = blocks' }
    _ -> is
interpretMove (Merge blockId1 blockId2) = modify' $ \is ->
  let blocks = isBlocks is
  in case (blockId1 `M.lookup` blocks, blockId2 `M.lookup` blocks) of
    (Just shape1, Just shape2) ->
      let lastBlockId' = isLastBlockId is + 1
          newBlockId = BlockId [lastBlockId']
          newShape =
            if (rX shape1) == (rX shape2)
              then Rectangle (rX shape1) (minimum [rY shape1, rY shape2]) (rWidth shape1) (rHeight shape1 + rHeight shape2)
              else Rectangle (minimum [rX shape1, rX shape2]) (rY shape1) (rWidth shape1 + rWidth shape2) (rHeight shape1)
          blocks' =
              M.insert newBlockId newShape
            $ M.delete blockId1
            $ M.delete blockId2
            $ isBlocks is
      in is { isLastBlockId = lastBlockId', isBlocks = blocks' }
    _ -> is

copyShape :: Image PixelRGBA8 -> Shape -> MutableImage s PixelRGBA8 -> Shape -> ST s ()
copyShape srcImage srcShape dstImage dstShape = do
  -- We have (0, 0) at bottom left, Juicy Pixels has it at top left
  let h = mutableImageHeight dstImage
  forM_ [0 .. (rHeight srcShape - 1)] $ \dy -> do
    let srcY = h - 1 - (rY srcShape + dy)
    let dstY = h - 1 - (rY dstShape + dy)
    forM_ [0 .. (rWidth srcShape - 1)] $ \dx -> do
      let srcX = rX srcShape + dx
      let dstX = rX dstShape + dx
      let pixel = pixelAt srcImage srcX srcY
      writePixel dstImage dstX dstY pixel
