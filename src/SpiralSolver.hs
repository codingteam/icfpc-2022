module SpiralSolver where

import Data.List (minimumBy)
import Data.Ord (comparing)
  
import Control.Monad.State

import GHC.Exts(groupWith)
import PNG(readPng)
import Types
import AST

type ProgramM a = State Program a

putMove :: Move -> ProgramM ()
putMove m = modify $ \p -> p ++ [m]

putColor :: Color -> Block -> ProgramM () 
putColor color block =
  putMove $ SetColor block color

getDominatingColor :: [(Point, Color)] -> Color
getDominatingColor picture =
  fst $ minimumBy (comparing snd) (map (\l -> (head l, length l)) $ groupWith id $ map snd picture)
    
drawBySpiral :: Block -> [(Point, Color)] -> ProgramM ()
drawBySpiral block currentImage = do
  let dominatingColor = getDominatingColor currentImage
  putColor dominatingColor block

drawPng :: FilePath -> IO Program
drawPng path = do
  putStrLn "Hellow"
  (width, height, picture) <- readPng path
  let root = Left $ SimpleBlock (BlockId [0]) (Rectangle 0 0 (width-1) (height-1)) white
  let (_, program) = runState (drawBySpiral root picture) []
  return program

