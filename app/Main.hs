module Main where

import qualified Data.Text.IO as TIO
import System.Environment

import DummySolver
import PNG
import Printer
import qualified Alt.Evaluator as AltEvaluator
import qualified Alt.Reader as AltReader
import qualified SpiralSolver

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      program <- drawPng path
      TIO.putStr $ printProgram program

    ["spiral", path] -> do
      program <- SpiralSolver.drawPng path
      TIO.putStr $ printProgram program

    ["average", path] -> do
        program <- drawPngAvgColor path
        TIO.putStr $ printProgram program

    ["average4", path] -> do
        program <- drawPngAvgQuadsColor path
        TIO.putStr $ printProgram program

    ["quads", ls, path] -> do
        let level = read ls
        program <- drawPngAverageQuads path level
        TIO.putStr $ printProgram program

    ["recursive", path] -> do
        program <- solveRecursive path
        TIO.putStr $ printProgram program

    ["evaluateSolution", imagePath, solutionPath] -> do
        image <- readPngImage imagePath
        program <- AltReader.readProgramFromFile solutionPath
        let result = AltEvaluator.evaluateProgram image program
        putStrLn $ concat ["The cost of this solution is ", show (AltEvaluator.erCost result)]

    _ -> putStrLn $ unlines [
              "Usage:"
            , "- <imagePath> - dun rummy solver"
            , "- spiral <imagePath> - run spiral solver"
            , "- evaluateSolution <imagePath> <solutionPath> - print out the cost of the solution"
            ]

