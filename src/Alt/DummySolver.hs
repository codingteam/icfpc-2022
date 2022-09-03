
module Alt.DummySolver where

import Control.Monad
import Control.Monad.State

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
  let blocksCfg = getBlocks cfg
      paint (blockId, shape) = SetColor blockId (calcAvgColor img shape)
  return $ map paint blocksCfg

