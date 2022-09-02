
module DummySolver where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M

import PNG(readPng)
import Types
import AST
import Interpreter

type ProgramM a = State Program a

putMove :: Move -> ProgramM ()
putMove m = modify $ \p -> m : p

cutToPixels :: Block -> ProgramM [Block]
cutToPixels block = do
  let shape = blockShape block
      middleX = rX shape + (rWidth shape `div` 2)
      middleY = rY shape + (rHeight shape `div` 2)
  if rWidth shape > 1 && rHeight shape > 1
    then do
         let children = cutPoint block (Point middleX middleY)
         putMove $ PointCut block (Point middleX middleY)
         concat <$> mapM cutToPixels children
    else if rWidth shape > 1
           then do
                let children = cutLine block Vertical middleX
                putMove $ LineCut block Vertical middleX
                concat <$> mapM cutToPixels children
           else if rHeight shape > 1
                  then do
                       let children = cutLine block Horizontal middleY
                       putMove $ LineCut block Horizontal middleY
                       concat <$> mapM cutToPixels children
                  else return [block]

-- | This implementation suits for 1x1 blocks only
buildMap :: [Block] -> M.Map Point Block
buildMap blocks = M.fromList [(Point (rX (blockShape b)) (rY (blockShape b)), b) | b <- blocks]

findBlock :: M.Map Point Block -> Point -> Block
findBlock blocks point =
  case M.lookup point blocks of
    Nothing -> error $ "Can't find point: " ++ show point
    Just block -> block

drawByPixels :: Block -> [(Point, Color)] -> ProgramM ()
drawByPixels root colors = do
  pixels <- cutToPixels root
  let blockMap = buildMap pixels
  forM_ colors $ \(point, color) -> do
    let block = findBlock blockMap point
    putMove (SetColor block color)

drawPng :: FilePath -> IO Program
drawPng path = do
  (width, height, picture) <- readPng path
  let root = Left $ SimpleBlock (BlockId [0]) (Rectangle 0 0 (width-1) (height-1)) white
  let (_, program) = runState (drawByPixels root picture) []
  return $ reverse program

