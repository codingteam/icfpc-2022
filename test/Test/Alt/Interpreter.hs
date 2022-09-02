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
altInterpreterTests =
  let rootBlockId = BlockId [0]
  in testGroup "Alt.Interpreter"
  [
    testCase "Empty program leaves canvas unchanged" $ do
      let state = initialState (400, 400)
      let initialCanvas = isBlocks state
      let result = execState (interpretProgram []) state
      (isBlocks result) @?= initialCanvas

  , testCase "Can change the colour of the canvas" $ do
      let black = PixelRGBA8 0 0 0 255
      let p = [SetColor rootBlockId black]
      let result = execState (interpretProgram p) (initialState (400, 400))

      (bColor $ fromJust $ rootBlockId `M.lookup` (isBlocks result)) @?= black

  , testCase "Can cut canvas horizontally" $ do
      let p = [LineCut rootBlockId Horizontal 8]
      let result = execState (interpretProgram p) (initialState (400, 400))

      let resultBlocks = isBlocks result
      (M.size resultBlocks) @?= 2

  , testCase "Can cut canvas vertically" $ do
      let p = [LineCut rootBlockId Vertical 399]
      let result = execState (interpretProgram p) (initialState (400, 400))

      let resultBlocks = isBlocks result
      (M.size resultBlocks) @?= 2

  , testCase "Can cut canvas at a point" $ do
      let canvasCenter = Point { pX = 200, pY = 200 }
      let p = [PointCut rootBlockId canvasCenter]
      let result = execState (interpretProgram p) (initialState (400, 400))

      let resultBlocks = isBlocks result
      (M.size resultBlocks) @?= 4
  ]
