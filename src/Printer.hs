{-# LANGUAGE OverloadedStrings #-}

module Printer (printProgram) where

import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU

import Types
import AST

printProgram :: Program -> T.Text
printProgram p = T.unlines $ map printMove p

printBlockId :: BlockId -> T.Text
printBlockId (BlockId ids) = T.intercalate "." $ map (T.pack . show) $ VU.toList ids

printBlock :: Block -> T.Text
printBlock b = "[" <> printBlockId (blockId b) <> "]"

printPoint :: Point -> T.Text
printPoint p = "[" <> T.pack (show $ pX p) <> "," <> T.pack (show $ pY p) <> "]"

printOrientation :: Orientation -> T.Text
printOrientation Vertical = "x"
printOrientation Horizontal = "y"

printColor :: Color -> T.Text
printColor (PixelRGBA8 r g b a) = "[" <> (T.intercalate "," $ map (T.pack . show) [r, g, b, a]) <> "]"

printMove :: Move -> T.Text
printMove (PointCut block point) = "cut " <> printBlock block <> " " <> printPoint point
printMove (LineCut block o line) = "cut " <> printBlock block <> " [" <> printOrientation o <> "] [" <> T.pack (show line) <> "]"
printMove (SetColor block color) = "color " <> printBlock block <> " " <> printColor color
printMove (Swap block1 block2) = "swap " <> printBlock block1 <> " " <> printBlock block2
printMove (Merge block1 block2) = "merge "<> printBlock block1 <> " " <> printBlock block2

