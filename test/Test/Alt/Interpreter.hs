module Test.Alt.Interpreter where

import Test.Tasty
import Test.Tasty.HUnit

import Codec.Picture.Types (pixelAt)
import Control.Monad.State (execState)
import qualified Data.Map as M

import Alt.AST
import Alt.Interpreter
import Types

altInterpreterTests :: TestTree
altInterpreterTests =
  let rootBlockId = BlockId [0]
      black = PixelRGBA8 0 0 0 255
  in testGroup "Alt.Interpreter"
  [
    testCase "Empty program leaves canvas unchanged" $ do
      let state = initialState (400, 400)
      let initialCanvas = isBlocks state
      let result = execState (interpretProgram []) state
      (isBlocks result) @?= initialCanvas

  , testCase "Can change the color of the canvas" $ do
      let p = [SetColor rootBlockId black]
      let result = execState (interpretProgram p) (initialState (400, 400))

      let image = isImage result
      let corner = pixelAt image 0 0

      corner @?= black

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

  , testCase "Can swap two areas of the canvas" $ do
      let p1 =
            [
              LineCut rootBlockId Horizontal 200
            , SetColor (BlockId [0, 0]) black
            ]
      let intermediate = execState (interpretProgram p1) (initialState (400, 400))

      let intermediateImage = isImage intermediate
      (pixelAt intermediateImage 0 0) @?= white

      let p2 = [Swap (BlockId [0, 0]) (BlockId [1, 0])]
      let final = execState (interpretProgram p2) intermediate

      let finalImage = isImage final
      (pixelAt finalImage 0 0) @?= black

  , testCase "Can merge two blocks into a single, new one" $ do
      let p1 = [LineCut rootBlockId Horizontal 200]
      let intermediate = execState (interpretProgram p1) (initialState (400, 400))

      (M.size $ isBlocks intermediate) @?= 2

      let p2 = [Merge (BlockId [0, 0]) (BlockId [1, 0])]
      let final = execState (interpretProgram p2) intermediate

      (M.size $ isBlocks final) @?= 1
      let expectedFinalShape = Rectangle 0 0 400 400
      ((BlockId [1]) `M.lookup` (isBlocks final)) @?= Just expectedFinalShape
  ]
