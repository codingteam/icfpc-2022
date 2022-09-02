module Test.Alt.Interpreter where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.State (execState)
import Data.Maybe (fromJust)
import qualified Data.Map as M

import Alt.Interpreter
import Alt.AST
import Alt.Types

altInterpreterTests :: TestTree
altInterpreterTests = testGroup "Alt.Interpreter"
  [
    testCase "Empty program leaves canvas unchanged" $ do
      let state = initialState (400, 400)
      let initialCanvas = isBlocks state
      let result = execState (interpretProgram []) state
      (isBlocks result) @?= initialCanvas

  , testCase "Can change the colour of the canvas" $ do
      let rootBlockId = BlockId [0]
      let black = PixelRGBA8 0 0 0 255
      let p = [SetColor rootBlockId black]
      let result = execState (interpretProgram p) (initialState (400, 400))

      (bColor $ fromJust $ rootBlockId `M.lookup` (isBlocks result)) @?= black
  ]
