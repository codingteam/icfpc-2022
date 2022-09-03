
module Alt.DummySolver where

import Control.Monad
import Control.Monad.State
import Data.Maybe (mapMaybe)

import Codec.Picture.Types

import Types
import PNG (readPng, readPngImage, calcAvgColor, subImage)
import Alt.AST
import Json

-- type ProgramM a = State Program a
-- 
-- putMove :: Move -> ProgramM ()
-- putMove m = modify $ \p -> m : p

paintWithAvgColors :: FilePath -> FilePath -> IO Program
paintWithAvgColors cfgPath imgPath = do
  img <- readPngImage imgPath
  cfg <- parseConfig cfgPath
  let blocksCfg = getColoredBlocks cfg
      paint (blockId, shape, existingColor) =
        let avgColor = calcAvgColor img shape
        in  if avgColor == existingColor
              then Nothing
              else Just $ SetColor blockId avgColor
  return $ mapMaybe paint blocksCfg

