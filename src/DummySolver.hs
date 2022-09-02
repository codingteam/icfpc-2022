
module DummySolver where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M

import Types
import AST
import Interpreter

type ProgramM a = State Program a

putMove :: Move -> ProgramM ()
putMove m = modify $ \p -> p ++ [m]

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
findBlock :: [Block] -> Point -> Block
findBlock blocks point =
  let arr = M.fromList [((rX (blockShape b), rY (blockShape b)), b) | b <- blocks]
  in  arr M.! (pX point, pY point)

drawByPixels :: Block -> [(Point, Color)] -> ProgramM ()
drawByPixels root colors = do
  pixels <- cutToPixels root
  forM_ colors $ \(point, color) -> do
    let block = findBlock pixels point
    putMove (SetColor block color)
  
