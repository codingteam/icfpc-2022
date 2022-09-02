module Main where

import Control.Monad

import Types
import AST
import DummySolver

main :: IO ()
main = do
  let root = Left $ SimpleBlock (BlockId [0]) (Rectangle 0 0 3 3) transparent
      pixels = cutToPixels root
  forM_ pixels print
