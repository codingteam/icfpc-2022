module Alt.Evaluator where

import Codec.Picture.Types (Image (..))
import Control.DeepSeq (deepseq)
import Control.Monad.State (execState)

import Alt.AST
import Alt.Interpreter
import Evaluator
import Types

data EvaluationResult = EvaluationResult {
    erImage :: Image PixelRGBA8
  , erCost :: !Integer
  }

evaluateProgram :: Image PixelRGBA8 -> Program -> EvaluationResult
evaluateProgram image program =
  let start = initialState (imageWidth image, imageHeight image)
      finish = execState (interpretProgram program) start

      finalImage = isImage finish
      totalCost = (isCost finish) + (fromIntegral $ imageSimilarity image finalImage)
  in finalImage `deepseq` totalCost `deepseq` EvaluationResult {
        erImage = finalImage
      , erCost = totalCost
      }
