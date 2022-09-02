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
  [path] <- getArgs
  program <- drawPng path
  putStrLn "Program:"
  TIO.putStr $ printProgram program

