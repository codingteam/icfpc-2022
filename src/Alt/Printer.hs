{-# LANGUAGE OverloadedStrings #-}

module Alt.Printer (printProgram) where

import qualified Data.Text as T

import Types
import Alt.AST

printProgram :: Program -> T.Text
printProgram p = T.unlines $ map printMove p

printBlockId :: BlockId -> T.Text
printBlockId (BlockId ids) = T.intercalate "." $ map (T.pack . show) $ reverse ids

printPoint :: Point -> T.Text
printPoint p = "[" <> T.pack (show $ pX p) <> "," <> T.pack (show $ pY p) <> "]"

printOrientation :: Orientation -> T.Text
printOrientation Vertical = "x"
printOrientation Horizontal = "y"

printColor :: Color -> T.Text
printColor (PixelRGBA8 r g b a) = "[" <> (T.intercalate "," $ map (T.pack . show) [r, g, b, a]) <> "]"

printMove :: Move -> T.Text
printMove (PointCut block point) = "cut " <> printBlockId block <> " " <> printPoint point
printMove (LineCut block o line) = "cut " <> printBlockId block <> " [" <> printOrientation o <> "] [" <> T.pack (show line) <> "]"
printMove (SetColor block color) = "color " <> printBlockId block <> " " <> printColor color
printMove (Swap block1 block2) = "swap " <> printBlockId block1 <> " " <> printBlockId block2
printMove (Merge block1 block2) = "merge "<> printBlockId block1 <> " " <> printBlockId block2

