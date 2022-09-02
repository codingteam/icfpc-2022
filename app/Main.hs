module Main where

import Control.Monad
import Control.Monad.State
import qualified Data.Text.IO as TIO
import System.Environment

import Types
import AST
import DummySolver
import Printer

main :: IO ()
main = do
    [cmd, path] <- getArgs
    case cmd of
      "average" -> do
        program <- drawPngAvgColor path
        TIO.putStr $ printProgram program

      "bypixel" -> do
        program <- drawPng path
        TIO.putStr $ printProgram program

      _ -> fail "unsupported command"

