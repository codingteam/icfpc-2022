module Main where

import Control.Monad
import Control.Monad.State
import qualified Data.Text.IO as TIO

import Types
import AST
import DummySolver
import Printer

main :: IO ()
main = do
  let root = Left $ SimpleBlock (BlockId [0]) (Rectangle 0 0 3 3) transparent
      picture = [
          (Point 0 0, PixelRGBA8 255 0 0 255),
          (Point 1 1, PixelRGBA8 0 255 0 255),
          (Point 2 2, PixelRGBA8 0 0 255 255)
        ]
      (pixels, program) = runState (drawByPixels root picture) []
  putStrLn "Program:"
  TIO.putStr $ printProgram program
  --putStrLn "Blocks:"
  --forM_ pixels print
