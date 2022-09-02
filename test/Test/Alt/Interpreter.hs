module Test.Alt.Interpreter where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.State (execState)

import Alt.Interpreter

altInterpreterTests :: TestTree
altInterpreterTests = testGroup "Alt.Interpreter"
  [
    testCase "Empty program leaves canvas unchanged" $ do
      let state = initialState (400, 400)
      let initialCanvas = isBlocks state
      let result = execState (interpretProgram []) state
      (isBlocks result) @?= initialCanvas
  ]
