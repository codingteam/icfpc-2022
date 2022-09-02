module Main where

import Control.Monad
import Control.Monad.State
import qualified Data.Text.IO as TIO
import System.Environment

import Types
import AST
import DummySolver
import Printer
import qualified SpiralSolver

main :: IO ()
main = do
--   let root = Left $ SimpleBlock (BlockId [0]) (Rectangle 0 0 400 400) transparent
--   let (pixels, _) = runState (cutToPixels root) []
--   forM_ pixels $ \b ->
--     print (rX (blockShape b), rY (blockShape b))
  args <- getArgs
  case args of
    [path] -> do
      program <- drawPng path
      putStrLn "Program:"
      TIO.putStr $ printProgram program
    ["spiral", path] -> do
      program <- SpiralSolver.drawPng path
      putStrLn "Program:"
      TIO.putStr $ printProgram program
    _ -> putStrLn "Usage:\n- <imagePath> - dun rummy solver\n- spiral <imagePath> - run spiral solver"
--     (w,h, picture) <- readPng path
--     pixels <- cutToPixels root
--     let blockMap = buildMap pixels

