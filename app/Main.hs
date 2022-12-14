module Main where

import Codec.Picture.Extra (beside)
import Codec.Picture.Png (writePng)
import System.Environment
import qualified Data.Text.IO as TIO

import Types (Orientation (..))
import Alt.DummySolver
import Alt.SolverM
import DummySolver
import Json (parseConfig)
import PNG
import Printer
import qualified Alt.Evaluator as AltEvaluator
import qualified Alt.Interpreter as AltInterpreter
import qualified Alt.Printer
import qualified Alt.Reader as AltReader
import qualified Alt.Solver.Billboard
import qualified SpiralSolver
import qualified Editor

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      program <- drawPng path
      TIO.putStr $ printProgram program

    ["editor", path] -> Editor.main path

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

    ["search4billboard", path] -> do
        program <- runSolverSimpleM path paintByQuadsSearchBillboard
        TIO.putStr $ Alt.Printer.printProgram program

    ["quads", ls, path] -> do
        let level = read ls
        program <- drawPngAverageQuads path False level
        TIO.putStr $ printProgram program

    ["quadsMerge", ls, path] -> do
        let level = read ls
        program <- runSolverSimpleM path (paintByQuadsAndMerge level 2)
        TIO.putStr $ Alt.Printer.printProgram program

    ["quadsSearchMerge", ls, path] -> do
        let level = read ls
        program <- runSolverSimpleM path (paintByQuadsSearchAndMerge level 2)
        TIO.putStr $ Alt.Printer.printProgram program

    ["quadsSearch2", ls, path] -> do
        let level = read ls
        program <- runSolverSimpleM path (paintByQuadsSearch2 level)
        TIO.putStr $ Alt.Printer.printProgram program

    ["quads-reset", ls, path] -> do
        let level = read ls
        program <- drawPngAverageQuads path True level
        TIO.putStr $ printProgram program

    ["recursive", path] -> do
        program <- solveRecursive path
        TIO.putStr $ printProgram program

    ["recursiveMerge", path] -> do
        program <- runSolverSimpleM path (solveRecursiveAndMerge 2)
        TIO.putStr $ Alt.Printer.printProgram program

    ["recursiveHalfs", path, ls] -> do
        let level = read ls
        program <- runSolverSimpleM path (recursiveHalfs2 level Vertical)
        TIO.putStr $ Alt.Printer.printProgram program

    ["dumbFromInitial", cfgPath, imgPath] -> do
        program <- paintWithAvgColors cfgPath imgPath
        TIO.putStr $ Alt.Printer.printProgram program

    ["mergeFromInitial", cfgPath, imgPath, tol] -> do
        let tolerance = read tol
        program <- runSolverM cfgPath imgPath (paintWithAvgColorsMerged tolerance)
        TIO.putStr $ Alt.Printer.printProgram program

    ["repaintQuadsMerge", cfgPath, imgPath, ls, tol] -> do
        let tolerance = read tol
        let level = read ls
        program <- runSolverM cfgPath imgPath (\cfg -> repaintByQuadsAndMerge level tolerance)
        TIO.putStr $ Alt.Printer.printProgram program

    ["repaintRecursiveHalfs", cfgPath, imgPath, ls] -> do
        let level = read ls
        program <- runSolverM cfgPath imgPath (\cfg -> repaintRecursiveHalfs level Vertical)
        TIO.putStr $ Alt.Printer.printProgram program

    ["repaintSearchBillboard", cfgPath, imgPath] -> do
        program <- runSolverM cfgPath imgPath (\cfg -> repaintByQuadsSearchBillboard)
        TIO.putStr $ Alt.Printer.printProgram program

    ["repaintQuadsSearch", cfgPath, imgPath, ls, tol] -> do
        let tolerance = read tol
        let level = read ls
        program <- runSolverM cfgPath imgPath (\cfg -> repaintByQuadsSearchAndMerge level tolerance)
        TIO.putStr $ Alt.Printer.printProgram program

    ["billboard", imgPath] -> do
        program <- runSolverSimpleM imgPath Alt.Solver.Billboard.solve
        TIO.putStr $ Alt.Printer.printProgram program

    ["billboardFromInitial", cfgPath, imgPath] -> do
        program <- runSolverM cfgPath imgPath Alt.Solver.Billboard.solveWithConfig
        TIO.putStr $ Alt.Printer.printProgram program

    -- TODO: properly parse optional arguments
    ["evaluateSolution", imagePath, solutionPath] -> do
      evaluateSolution imagePath solutionPath Nothing Nothing

    ["evaluateSolution", imagePath, solutionPath, ""] -> do
      evaluateSolution imagePath solutionPath Nothing Nothing

    ["evaluateSolution", imagePath, solutionPath, imageComparisonPath] -> do
      evaluateSolution imagePath solutionPath (Just imageComparisonPath) Nothing

    ["evaluateSolution", imagePath, solutionPath, "", cfgPath] -> do
      evaluateSolution imagePath solutionPath Nothing (Just cfgPath)

    ["evaluateSolution", imagePath, solutionPath, imageComparisonPath, cfgPath] -> do
      evaluateSolution imagePath solutionPath (Just imageComparisonPath) (Just cfgPath)

    ["parseConfig", path] -> do
      cfg <- parseConfig path
      print cfg

    _ -> putStrLn $ unlines [
              "Usage:"
            , "- <imagePath> - dun rummy solver"
            , "- spiral <imagePath> - run spiral solver"
            , "- evaluateSolution <imagePath> <solutionPath> [<comparisonImagePath>] - print out the cost of the solution, and optionally write a visual comparison between the problem and the solution"
            ]

evaluateSolution :: FilePath -> FilePath -> Maybe FilePath -> Maybe FilePath -> IO ()
evaluateSolution imagePath solutionPath imageComparisonPath cfgPath = do
  image <- readPngImage imagePath
  program <- AltReader.readProgramFromFile solutionPath
  cfg <- sequence $ parseConfig <$> cfgPath
  let initialState = AltInterpreter.initialStateFromJson <$> cfg
  let result = AltEvaluator.evaluateProgram image program initialState
  case imageComparisonPath of
    Nothing -> return ()
    Just p -> do
      let comparisonImg = beside [image, AltEvaluator.erImage result]
      writePng p comparisonImg
  putStrLn $ show $ AltEvaluator.erCost result
