{-# LANGUAGE OverloadedStrings #-}

module Alt.Reader (readProgramFromFile, readProgram) where

import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector.Unboxed as VU

import Alt.AST
import Types

readProgramFromFile :: FilePath -> IO Program
readProgramFromFile path = do
  text <- TIO.readFile path
  return $ readProgram text

readProgram :: T.Text -> Program
readProgram text = reverse $ foldl' readLine [] (T.lines text)
  where
  readLine :: Program -> T.Text -> Program
  readLine revProgram "" = revProgram
  readLine revProgram line =
    case T.uncons line of
      Just ('#', _comment) -> revProgram
      Just ('\n', _comment) -> revProgram
      _ -> (readMove $ T.words line) : revProgram

readMove :: [T.Text] -> Move
readMove ["cut", bId, point] = PointCut (readBlockId bId) (readPoint point)
readMove ["cut", bId, orientation, lineNo] = LineCut (readBlockId bId) (readOrientation orientation) (readNumber lineNo)
readMove ["color", bId, color] = SetColor (readBlockId bId) (readColor color)
readMove ["swap", bId1, bId2] = Swap (readBlockId bId1) (readBlockId bId2)
readMove ["merge", bId1, bId2] = Merge (readBlockId bId1) (readBlockId bId2)
readMove move = error $ "Could not parse this move: <" ++ (T.unpack $ T.unwords move) ++ ">"

readBlockId :: T.Text -> BlockId
readBlockId bId =
  let Just ('[', rest) = T.uncons bId
      Just (rest', ']') = T.unsnoc rest
      numbers = T.splitOn "." rest'
      numbers' = map parseNumber numbers
  in BlockId (VU.fromList numbers')

readPoint :: T.Text -> Point
readPoint point =
  let Just ('[', rest) = T.uncons point
      Just (rest', ']') = T.unsnoc rest
      [textX, textY] = T.splitOn "," rest'
      x = parseNumber textX
      y = parseNumber textY
  in Point { pX = x, pY = y }

readOrientation :: T.Text -> Orientation
readOrientation orientation =
  let Just ('[', rest) = T.uncons orientation
      Just (rest', ']') = T.unsnoc rest
  in helper rest'
  where
  helper "x" = Vertical
  helper "X" = Vertical
  helper "y" = Horizontal
  helper "Y" = Horizontal
  helper o = error $ "Could not parse orientation: " ++ (T.unpack o)

readNumber :: T.Text -> Int
readNumber number =
  let Just ('[', rest) = T.uncons number
      Just (rest', ']') = T.unsnoc rest
  in parseNumber rest'

parseNumber :: T.Text -> Int
parseNumber = read . T.unpack

readColor :: T.Text -> Color
readColor color =
  let Just ('[', rest) = T.uncons color
      Just (rest', ']') = T.unsnoc rest
      channels = T.splitOn "," rest'
      [r, g, b, a] = map (fromIntegral . parseNumber) channels
  in PixelRGBA8 r g b a
