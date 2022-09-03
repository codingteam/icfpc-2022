{-# LANGUAGE RecordWildCards #-}

module Alt.Interpreter where

import Codec.Picture.Types
import Control.Monad
import Control.Monad.ST (ST, runST)
import Control.Monad.State
import qualified Data.Map as M

import Alt.AST
import Types
import Util

data InterpreterState = InterpreterState {
    isLastBlockId :: Int
  , isBlocks :: M.Map BlockId Shape
  , isImage :: Image PixelRGBA8
  , isCost :: Int
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
      , isCost = 0
      }

interpretProgram :: Program -> InterpretM ()
interpretProgram p = forM_ p interpretMove

interpretMove :: Move -> InterpretM ()
interpretMove (PointCut bId (Point x y)) = modify' $ \is ->
  case bId `M.lookup` (isBlocks is) of
    Nothing -> is
    Just parent ->
      let dx = x - rX parent
          dy = y - rY parent
          bottomLeft = Rectangle (rX parent) (rY parent) dx dy
          bottomRight = Rectangle (rX parent + dx) (rY parent) (rWidth parent - dx) dy
          topRight = Rectangle (rX parent + dx) (rY parent + dy) (rWidth parent - dx) (rHeight parent - dy)
          topLeft = Rectangle (rX parent) (rY parent + dy) dx (rHeight parent - dy)

          blocks' =
              M.insert (bId +. 0) bottomLeft
            $ M.insert (bId +. 1) bottomRight
            $ M.insert (bId +. 2) topRight
            $ M.insert (bId +. 3) topLeft
            $ M.delete bId
            $ isBlocks is
          cost = calculateMoveCost (isImage is) 10 parent
      in is { isBlocks = blocks', isCost = cost + isCost is }
interpretMove (LineCut bId Horizontal y) = modify' $ \is ->
  case bId `M.lookup` (isBlocks is) of
    Nothing -> is
    Just parent ->
      let dy = y - rY parent
          top = Rectangle (rX parent) (rY parent + dy) (rWidth parent) (rHeight parent - dy)
          bottom = Rectangle (rX parent) (rY parent) (rWidth parent) dy
          blocks' =
              M.insert (bId +. 0) bottom
            $ M.insert (bId +. 1) top
            $ M.delete bId
            $ isBlocks is
          cost = calculateMoveCost (isImage is) 7 parent
      in is { isBlocks = blocks', isCost = cost + isCost is }
interpretMove (LineCut bId Vertical x) = modify' $ \is ->
  case bId `M.lookup` (isBlocks is) of
    Nothing -> is
    Just parent ->
      let dx = x - rX parent
          left = Rectangle (rX parent) (rY parent) dx (rHeight parent)
          right = Rectangle (rX parent + dx) (rY parent) (rWidth parent - dx) (rHeight parent)
          blocks' =
              M.insert (bId +. 0) left
            $ M.insert (bId +. 1) right
            $ M.delete bId
            $ isBlocks is
          cost = calculateMoveCost (isImage is) 7 parent
      in is { isBlocks = blocks', isCost = cost + isCost is }
interpretMove (SetColor bId color) = modify' $ \is ->
  case bId `M.lookup` (isBlocks is) of
    Nothing -> is
    Just shape@(Rectangle { .. }) ->
      let image = isImage is
          image' = runST $ do
            img <- thawImage image

            -- We have (0, 0) at bottom left, Juicy Pixels has it at top left
            let h = mutableImageHeight img
            forM_ [(h - rY - rHeight) .. (h - rY - 1)] $ \y -> do
              forM_ [rX .. (rX + rWidth - 1)] $ \x -> do
                writePixel img x y color

            freezeImage img
          cost = calculateMoveCost image 5 shape
      in  is { isImage = image', isCost = cost + isCost is }
interpretMove (Swap bId1 bId2) = modify' $ \is ->
  let blocks = isBlocks is
  in case (bId1 `M.lookup` blocks, bId2 `M.lookup` blocks) of
    (Just shape1, Just shape2) ->
      let image = isImage is
          image' = runST $ do
            img <- thawImage image

            copyShape image shape1 img shape2
            copyShape image shape2 img shape1

            freezeImage img
          blocks' =
              M.insert bId1 shape2
            $ M.insert bId2 shape1
            $ isBlocks is
          cost = calculateMoveCost (isImage is) 3 shape1
      in is { isImage = image', isBlocks = blocks', isCost = cost + isCost is }
    _ -> is
interpretMove (Merge bId1 bId2) = modify' $ \is ->
  let blocks = isBlocks is
  in case (bId1 `M.lookup` blocks, bId2 `M.lookup` blocks) of
    (Just shape1, Just shape2) ->
      let lastBlockId' = isLastBlockId is + 1
          newBlockId = BlockId [lastBlockId']
          newShape =
            if (rX shape1) == (rX shape2)
              then Rectangle (rX shape1) (minimum [rY shape1, rY shape2]) (rWidth shape1) (rHeight shape1 + rHeight shape2)
              else Rectangle (minimum [rX shape1, rX shape2]) (rY shape1) (rWidth shape1 + rWidth shape2) (rHeight shape1)
          blocks' =
              M.insert newBlockId newShape
            $ M.delete bId1
            $ M.delete bId2
            $ isBlocks is
          -- Announcement from 02/09/2022, 21:35:00:
          -- When two blocks are merged, the cost is calculated by picking the
          -- larger block for computation.
          cost =
            if shapeArea shape1 > shapeArea shape2
              then calculateMoveCost (isImage is) 1 shape1
              else calculateMoveCost (isImage is) 1 shape2
      in is { isLastBlockId = lastBlockId', isBlocks = blocks', isCost = cost + isCost is }
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

calculateMoveCost :: Image a -> Int -> Shape -> Int
calculateMoveCost image baseCost shape =
  let canvasArea = (fromIntegral $ imageWidth image) * (fromIntegral $ imageHeight image)
      lhs = baseCost * canvasArea
      rhs = shapeArea shape
      fraction = (fromIntegral lhs :: Double) / (fromIntegral rhs)
  in jsRound fraction
