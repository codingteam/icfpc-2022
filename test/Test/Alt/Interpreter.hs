module Test.Alt.Interpreter where

import Test.Tasty
import Test.Tasty.HUnit

import Codec.Picture.Types (pixelAt)
import Control.Monad.State (execState)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector.Unboxed as VU

import Alt.AST
import Alt.Interpreter
import Types
import Util

altInterpreterTests :: TestTree
altInterpreterTests =
  let rootBlockId = createBlockId 0
      black = PixelRGBA8 0 0 0 255
  in testGroup "Alt.Interpreter"
  [
    testCase "Empty program leaves canvas unchanged, at zero cost" $ do
      let state = initialState (400, 400)
      let initialCanvas = isBlocks state
      let result = execState (interpretProgram []) state
      (isBlocks result) @?= initialCanvas
      (isCost result) @?= 0

  , testCase "Can change the color of the canvas" $ do
      let p = [SetColor rootBlockId black]
      let result = execState (interpretProgram p) (initialState (400, 400))

      let image = isImage result
      let corner = pixelAt image 0 0

      corner @?= black

      (isCost result) @?= 5

  , testCase "Can cut canvas horizontally" $ do
      let p = [LineCut rootBlockId Horizontal 8]
      let result = execState (interpretProgram p) (initialState (400, 400))

      let resultBlocks = isBlocks result
      (HMS.size resultBlocks) @?= 2

      (isCost result) @?= 7

  , testCase "Can cut canvas vertically" $ do
      let p = [LineCut rootBlockId Vertical 399]
      let result = execState (interpretProgram p) (initialState (400, 400))

      let resultBlocks = isBlocks result
      (HMS.size resultBlocks) @?= 2

      (isCost result) @?= 7

  , testCase "Can cut canvas at a point" $ do
      let canvasCenter = Point { pX = 200, pY = 200 }
      let p = [PointCut rootBlockId canvasCenter]
      let result = execState (interpretProgram p) (initialState (400, 400))

      let resultBlocks = isBlocks result
      (HMS.size resultBlocks) @?= 4

      (isCost result) @?= 10

  , testCase "Can swap two areas of the canvas" $ do
      let p1 =
            [
              LineCut rootBlockId Horizontal 200
            , SetColor (BlockId $ VU.fromList [0, 0]) black
            ]
      let intermediate = execState (interpretProgram p1) (initialState (400, 400))

      let intermediateImage = isImage intermediate
      (pixelAt intermediateImage 0 0) @?= white
      let intermediateCost = isCost intermediate

      let p2 = [Swap (BlockId $ VU.fromList [0, 0]) (BlockId $ VU.fromList [0, 1])]
      let final = execState (interpretProgram p2) intermediate

      let finalImage = isImage final
      (pixelAt finalImage 0 0) @?= black

      let expectedSwapCost = jsRound ((3 * (400 * 400)) / (400 * 200) :: Double)
      (isCost final - intermediateCost) @?= expectedSwapCost

  , testCase "Can merge two blocks into a single, new one" $ do
      let p1 = [LineCut rootBlockId Horizontal 100]
      let intermediate = execState (interpretProgram p1) (initialState (400, 400))

      (HMS.size $ isBlocks intermediate) @?= 2
      let intermediateCost = isCost intermediate

      let p2 = [Merge (BlockId $ VU.fromList [0, 0]) (BlockId $ VU.fromList [0, 1])]
      let final = execState (interpretProgram p2) intermediate

      (HMS.size $ isBlocks final) @?= 1
      let expectedFinalShape = Rectangle 0 0 400 400
      ((createBlockId 1) `HMS.lookup` (isBlocks final)) @?= Just expectedFinalShape

      -- Announcement from 02/09/2022, 21:35:00:
      -- When two blocks are merged, the cost is calculated by picking the
      -- larger block for computation.
      let expectedMergeCost = jsRound ((1 * (400 * 400)) / (400 * 300) :: Double)
      (isCost final - intermediateCost) @?= expectedMergeCost
  ]
