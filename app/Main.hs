module Main where

import Codec.Picture.Extra (beside)
import Codec.Picture.Png (writePng)
import System.Environment
import qualified Data.Text.IO as TIO

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

    ["search4", path] -> do
        program <- drawPngQuadsSearch path
        TIO.putStr $ printProgram program

    ["quads", ls, path] -> do
        let level = read ls
        program <- drawPngAverageQuads path False level
        TIO.putStr $ printProgram program

    ["quads-reset", ls, path] -> do
        let level = read ls
        program <- drawPngAverageQuads path True level
        TIO.putStr $ printProgram program

    ["recursive", path] -> do
        program <- solveRecursive path
        TIO.putStr $ printProgram program

    ["evaluateSolution", imagePath, solutionPath] -> do
      evaluateSolution imagePath solutionPath Nothing

    ["evaluateSolution", imagePath, solutionPath, imageComparisonPath] -> do
      evaluateSolution imagePath solutionPath (Just imageComparisonPath)

    _ -> putStrLn $ unlines [
              "Usage:"
            , "- <imagePath> - dun rummy solver"
            , "- spiral <imagePath> - run spiral solver"
            , "- evaluateSolution <imagePath> <solutionPath> [<comparisonImagePath>] - print out the cost of the solution, and optionally write a visual comparison between the problem and the solution"
            ]

evaluateSolution :: FilePath -> FilePath -> Maybe FilePath -> IO ()
evaluateSolution imagePath solutionPath imageComparisonPath = do
  image <- readPngImage imagePath
  program <- AltReader.readProgramFromFile solutionPath
  let result = AltEvaluator.evaluateProgram image program
  case imageComparisonPath of
    Nothing -> return ()
    Just p -> do
      let comparisonImg = beside [image, AltEvaluator.erImage result]
      writePng p comparisonImg
      putStrLn $ concat ["Comparison image written to ", p]
  putStrLn $ concat ["The cost of this solution is ", show (AltEvaluator.erCost result)]
