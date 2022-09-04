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
import Data.Maybe
import qualified Data.HashMap.Strict as HMS

import Alt.AST
import Types
import Util
import Json (Configuration (..), getBlocks, BlockJson (..))

data InterpreterState = InterpreterState {
    isLastBlockId :: !Int
  , isBlocks :: HMS.HashMap BlockId Shape
  , isImage :: Image PixelRGBA8
  , isCost :: !Integer
  , isMoveCostFn :: Move -> Integer
  }

type InterpretM a = State InterpreterState a

initialState' :: (Coordinate, Coordinate) -> [(BlockId, Shape)] -> (Move -> Integer) -> InterpreterState
initialState' (width, height) blocks moveCostFn =
  let whitePixel = PixelRGBA8 255 255 255 255
      image = runST $ do
        img <- createMutableImage width height whitePixel
        freezeImage img
  in  image `deepseq` InterpreterState {
        isLastBlockId = 0
      , isBlocks = HMS.fromList blocks
      , isImage = image
      , isCost = 0
      , isMoveCostFn = moveCostFn
      }

-- | Given canvas dimensions, create the initial state.
initialState :: (Coordinate, Coordinate) -> InterpreterState
initialState size@(width,height) = initialState' size [(createBlockId 0, Rectangle 0 0 width height)] oldMoveCostFn

-- FIXME :: this does not fill image in the state from configuration!
initialStateFromJson :: Configuration -> InterpreterState
initialStateFromJson cfg =
  let moveCostFn = if (isJust $ cSourcePngJson cfg) then newMoveCostFn else oldMoveCostFn
      st0 = (initialState' (cWidth cfg, cHeight cfg) (getBlocks cfg) moveCostFn) {isLastBlockId = length (cBlocks cfg)-1}
      st1 = execState (renderConfiguration cfg) st0
  in  st0 {isImage = isImage st1}

renderConfiguration :: Configuration -> InterpretM ()
renderConfiguration cfg = forM_ (cBlocks cfg) renderBlock
  where
    renderBlock block = fillBlock (bjId block) (bjColor block)

getImageSize :: InterpretM (Coordinate, Coordinate)
getImageSize = do
  img <- gets isImage
  return (imageWidth img, imageHeight img)

interpretProgram :: Program -> InterpretM ()
interpretProgram p = forM_ p interpretMove

interpretMove :: Move -> InterpretM ()
interpretMove move@(PointCut bId point) = getMoveCost move >>= interpretPointCut bId point
interpretMove move@(LineCut bId orientation y) = getMoveCost move >>= interpretLineCut bId orientation y
interpretMove move@(SetColor bId color) = getMoveCost move >>= interpretSetColor bId color
interpretMove move@(Swap bId1 bId2) = getMoveCost move >>= interpretSwap bId1 bId2
interpretMove move@(Merge bId1 bId2) = getMoveCost move >>= interpretMerge bId1 bId2

interpretPointCut :: BlockId -> Point -> Integer -> InterpretM ()
interpretPointCut bId (Point x y) moveBaseCost = do
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
  increaseCost moveBaseCost parent

interpretLineCut :: BlockId -> Orientation -> Coordinate -> Integer -> InterpretM ()
interpretLineCut bId Horizontal y moveBaseCost = do
  parent <- getBlock bId
  let dy = y - rY parent
  let top = Rectangle (rX parent) (rY parent + dy) (rWidth parent) (rHeight parent - dy)
  let bottom = Rectangle (rX parent) (rY parent) (rWidth parent) dy

  insertOrUpdateBlock (bId +. 0) bottom
  insertOrUpdateBlock (bId +. 1) top
  deleteBlock bId
  increaseCost moveBaseCost parent
interpretLineCut bId Vertical x moveBaseCost = do
  parent <- getBlock bId
  let dx = x - rX parent
  let left = Rectangle (rX parent) (rY parent) dx (rHeight parent)
  let right = Rectangle (rX parent + dx) (rY parent) (rWidth parent - dx) (rHeight parent)

  insertOrUpdateBlock (bId +. 0) left
  insertOrUpdateBlock (bId +. 1) right
  deleteBlock bId
  increaseCost moveBaseCost parent

fillBlock :: BlockId -> Color -> InterpretM ()
fillBlock bId color = do
  shape@(Rectangle { .. }) <- getBlock bId
  withImage $ \image -> do
    img <- thawImage image

    -- We have (0, 0) at bottom left, Juicy Pixels has it at top left
    let h = mutableImageHeight img
    forM_ [(h - rY - rHeight) .. (h - rY - 1)] $ \y -> do
      forM_ [rX .. (rX + rWidth - 1)] $ \x -> do
        writePixel img x y color

    unsafeFreezeImage img

interpretSetColor :: BlockId -> Color -> Integer -> InterpretM ()
interpretSetColor bId color moveBaseCost = do
  shape <- getBlock bId
  fillBlock bId color
  increaseCost moveBaseCost shape

interpretSwap :: BlockId -> BlockId -> Integer -> InterpretM ()
interpretSwap bId1 bId2 moveBaseCost = do
  shape1 <- getBlock bId1
  shape2 <- getBlock bId2
  withImage $ \image -> do
    img <- thawImage image

    copyShape image shape1 img shape2
    copyShape image shape2 img shape1

    freezeImage img
  insertOrUpdateBlock bId1 shape2
  insertOrUpdateBlock bId2 shape1
  increaseCost moveBaseCost shape1

interpretMerge :: BlockId -> BlockId -> Integer -> InterpretM ()
interpretMerge bId1 bId2 moveBaseCost = do
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
  increaseCost moveBaseCost largestShape

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
  let
      cost = calculateMoveCost (isImage is) baseCost shape
  in is { isCost = cost + isCost is }

calculateMoveCost :: Image a -> Integer -> Shape -> Integer
calculateMoveCost image baseCost shape =
  let canvasArea = (fromIntegral $ imageWidth image) * (fromIntegral $ imageHeight image)
      lhs = baseCost * canvasArea
      rhs = shapeArea shape
      fraction = (fromIntegral lhs :: Double) / (fromIntegral rhs)
  in force $ jsRound fraction

getMoveCost :: Move -> InterpretM Integer
getMoveCost move = do
  moveCostFn <- gets isMoveCostFn
  return $ moveCostFn move

oldMoveCostFn :: Move -> Integer
oldMoveCostFn (PointCut _ _) = 10
oldMoveCostFn (LineCut _ _ _) = 7
oldMoveCostFn (SetColor _ _) = 5
oldMoveCostFn (Swap _ _) = 3
oldMoveCostFn (Merge _ _) = 1

newMoveCostFn :: Move -> Integer
newMoveCostFn (PointCut _ _) = 3
newMoveCostFn (LineCut _ _ _) = 2
newMoveCostFn (SetColor _ _) = 5
newMoveCostFn (Swap _ _) = 3
newMoveCostFn (Merge _ _) = 1
