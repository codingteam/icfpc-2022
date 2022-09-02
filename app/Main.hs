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
--   let root = Left $ SimpleBlock (BlockId [0]) (Rectangle 0 0 400 400) transparent
--   let (pixels, _) = runState (cutToPixels root) []
--   forM_ pixels $ \b ->
--     print (rX (blockShape b), rY (blockShape b))
    [path] <- getArgs
    program <- drawPng path
    putStrLn "Program:"
    TIO.putStr $ printProgram program
--     (w,h, picture) <- readPng path
--     pixels <- cutToPixels root
--     let blockMap = buildMap pixels

