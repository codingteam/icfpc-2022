
module DummySolver where

import Types
import AST
import Interpreter

cutToPixels :: Block -> [Block]
cutToPixels block =
  let shape = blockShape block
      middleX = rX shape + (rWidth shape `div` 2)
      middleY = rY shape + (rHeight shape `div` 2)
  in  if rWidth shape > 1 && rHeight shape > 1
        then let children = cutPoint block (Point middleX middleY)
             in  concatMap cutToPixels children
        else if rWidth shape > 1
               then let children = cutLine block Vertical middleX
                    in  concatMap cutToPixels children
               else if rHeight shape > 1
                      then let children = cutLine block Horizontal middleY
                           in concatMap cutToPixels children
                      else [block]


