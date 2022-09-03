{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Alt.Interpreter where

import Codec.Picture.Types
  (Image (..), MutableImage (..), createMutableImage, writePixel, pixelAt,
  freezeImage, unsafeFreezeImage, thawImage)
import Control.DeepSeq (deepseq, force)
import Control.Monad
import Control.Monad.ST (ST, runST)
import Control.Monad.State
import qualified Data.HashMap.Strict as HMS

import Alt.AST
import Types
import Util
import Json (Configuration (..), getBlocks)

data InterpreterState = InterpreterState {
    isLastBlockId :: !Int
  , isBlocks :: HMS.HashMap BlockId Shape
  , isImage :: Image PixelRGBA8
  , isCost :: !Integer
  }

type InterpretM a = State InterpreterState a

initialState' :: (Coordinate, Coordinate) -> [(BlockId, Shape)] -> InterpreterState
initialState' (width, height) blocks =
  let whitePixel = PixelRGBA8 255 255 255 255
      image = runST $ do
        img <- createMutableImage width height whitePixel
        freezeImage img
  in  image `deepseq` InterpreterState {
        isLastBlockId = 0
      , isBlocks = HMS.fromList blocks
      , isImage = image
      , isCost = 0
      }

-- | Given canvas dimensions, create the initial state.
initialState :: (Coordinate, Coordinate) -> InterpreterState
initialState size@(width,height) = initialState' size [(createBlockId 0, Rectangle 0 0 width height)]

-- FIXME :: this does not fill image in the state from configuration!
initialStateFromJson :: Configuration -> InterpreterState
initialStateFromJson cfg = initialState' (cWidth cfg, cHeight cfg) (getBlocks cfg)

interpretProgram :: Program -> InterpretM ()
interpretProgram p = forM_ p interpretMove

interpretMove :: Move -> InterpretM ()
interpretMove (PointCut bId (Point x y)) = do
  parent <- getBlock bId
  let dx = x - rX parent
  let dy = y - rY parent
  let bottomLeft = Rectangle (rX parent) (rY parent) dx dy
  let bottomRight = Rectangle (rX parent + dx) (rY parent) (rWidth parent - dx) dy
  let topRight = Rectangle (rX parent + dx) (rY parent + dy) (rWidth parent - dx) (rHeight parent - dy)
  let topLeft = Rectangle (rX parent) (rY parent + dy) dx (rHeight parent - dy)

  insertOrUpdateBlock (bId +. 0) bottomLeft
  insertOrUpdateBlock (bId +. 1) bottomRight
  insertOrUpdateBlock (bId +. 2) topRight
  insertOrUpdateBlock (bId +. 3) topLeft
  deleteBlock bId
  increaseCost 10 parent
interpretMove (LineCut bId Horizontal y) = do
  parent <- getBlock bId
  let dy = y - rY parent
  let top = Rectangle (rX parent) (rY parent + dy) (rWidth parent) (rHeight parent - dy)
  let bottom = Rectangle (rX parent) (rY parent) (rWidth parent) dy

  insertOrUpdateBlock (bId +. 0) bottom
  insertOrUpdateBlock (bId +. 1) top
  deleteBlock bId
  increaseCost 7 parent
interpretMove (LineCut bId Vertical x) = do
  parent <- getBlock bId
  let dx = x - rX parent
  let left = Rectangle (rX parent) (rY parent) dx (rHeight parent)
  let right = Rectangle (rX parent + dx) (rY parent) (rWidth parent - dx) (rHeight parent)

  insertOrUpdateBlock (bId +. 0) left
  insertOrUpdateBlock (bId +. 1) right
  deleteBlock bId
  increaseCost 7 parent
interpretMove (SetColor bId color) = do
  shape@(Rectangle { .. }) <- getBlock bId
  withImage $ \image -> do
    img <- thawImage image

    -- We have (0, 0) at bottom left, Juicy Pixels has it at top left
    let h = mutableImageHeight img
    forM_ [(h - rY - rHeight) .. (h - rY - 1)] $ \y -> do
      forM_ [rX .. (rX + rWidth - 1)] $ \x -> do
        writePixel img x y color

    unsafeFreezeImage img
  increaseCost 5 shape
interpretMove (Swap bId1 bId2) = do
  shape1 <- getBlock bId1
  shape2 <- getBlock bId2
  withImage $ \image -> do
    img <- thawImage image

    copyShape image shape1 img shape2
    copyShape image shape2 img shape1

    freezeImage img
  insertOrUpdateBlock bId1 shape2
  insertOrUpdateBlock bId2 shape1
  increaseCost 3 shape1
interpretMove (Merge bId1 bId2) = do
  shape1 <- getBlock bId1
  shape2 <- getBlock bId2
  newBlockId <- getNewBlockId
  let newShape =
        if (rX shape1) == (rX shape2)
          then Rectangle (rX shape1) (minimum [rY shape1, rY shape2]) (rWidth shape1) (rHeight shape1 + rHeight shape2)
          else Rectangle (minimum [rX shape1, rX shape2]) (rY shape1) (rWidth shape1 + rWidth shape2) (rHeight shape1)

  insertOrUpdateBlock newBlockId newShape
  deleteBlock bId1
  deleteBlock bId2
  -- Announcement from 02/09/2022, 21:35:00:
  -- When two blocks are merged, the cost is calculated by picking the
  -- larger block for computation.
  let largestShape = if shapeArea shape1 > shapeArea shape2 then shape1 else shape2
  increaseCost 1 largestShape

-- | Returns a block with given id.
--
-- Throws an error if no such block exists.
getBlock :: BlockId -> InterpretM Shape
getBlock bId = do
  blocks <- gets isBlocks
  case bId `HMS.lookup` blocks of
    Nothing -> error $ "There is no block with id " ++ show bId
    Just shape -> return shape

insertOrUpdateBlock :: BlockId -> Shape -> InterpretM ()
insertOrUpdateBlock bId shape = modify' $ \is ->
  let blocks = HMS.insert bId shape (isBlocks is)
  in is { isBlocks = blocks }

-- | Removes a given block.
--
-- Throws an error if no such block exists.
deleteBlock :: BlockId -> InterpretM ()
deleteBlock bId = modify' $ \is ->
  let blocks = HMS.delete bId (isBlocks is)
  in is { isBlocks = blocks }

withImage :: (forall s. Image PixelRGBA8 -> ST s (Image PixelRGBA8)) -> InterpretM ()
withImage fn =
  modify' $ \is ->
    let image = isImage is
        newImage = runST (fn image)
    in newImage `deepseq` is { isImage = newImage }

getNewBlockId :: InterpretM BlockId
getNewBlockId = do
  lastBlockId <- gets isLastBlockId
  let result = createBlockId (lastBlockId + 1)
  modify' $ \s -> s { isLastBlockId = isLastBlockId s + 1 }
  return result

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

-- | Given a base cost and a shape, increase the current cost by an appropriate
-- amount.
increaseCost :: Integer -> Shape -> InterpretM ()
increaseCost baseCost shape = modify' $ \is ->
  let cost = calculateMoveCost (isImage is) baseCost shape
  in is { isCost = cost + isCost is }

calculateMoveCost :: Image a -> Integer -> Shape -> Integer
calculateMoveCost image baseCost shape =
  let canvasArea = (fromIntegral $ imageWidth image) * (fromIntegral $ imageHeight image)
      lhs = baseCost * canvasArea
      rhs = shapeArea shape
      fraction = (fromIntegral lhs :: Double) / (fromIntegral rhs)
  in force $ jsRound fraction
