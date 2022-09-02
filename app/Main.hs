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
      (pixels, program) = runState (cutToPixels root) []
  putStrLn "Program:"
  TIO.putStr $ printProgram program
  putStrLn "Blocks:"
  forM_ pixels print
