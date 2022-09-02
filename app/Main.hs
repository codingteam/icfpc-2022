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

    ["average", path] -> do
        program <- drawPngAvgColor path
        putStrLn "Program:"
        TIO.putStr $ printProgram program

    _ -> putStrLn "Usage:\n- <imagePath> - dun rummy solver\n- spiral <imagePath> - run spiral solver"

