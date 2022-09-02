import Test.Tasty

import Test.Alt.Interpreter

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [altInterpreterTests]
