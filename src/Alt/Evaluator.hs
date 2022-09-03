module Alt.Evaluator where

import Codec.Picture.Types (Image (..))
import Control.DeepSeq (deepseq)
import Control.Monad.State (execState)

import Alt.AST
import Alt.Interpreter
import Evaluator
import Types
import Data.Maybe (fromMaybe)

data EvaluationResult = EvaluationResult {
    erImage :: Image PixelRGBA8
  , erCost :: !Integer
  }

evaluateProgram :: Image PixelRGBA8 -> Program -> Maybe InterpreterState -> EvaluationResult
evaluateProgram image program start =
  let start' = fromMaybe (initialState (imageWidth image, imageHeight image)) start
      finish = execState (interpretProgram program) start'

      finalImage = isImage finish
      totalCost = (isCost finish) + (fromIntegral $ imageSimilarity image finalImage)
  in finalImage `deepseq` totalCost `deepseq` EvaluationResult {
        erImage = finalImage
      , erCost = totalCost
      }
