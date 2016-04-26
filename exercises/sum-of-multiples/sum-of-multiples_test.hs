import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import SumOfMultiples (sumOfMultiples)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList sumOfMultiplesTests ]

-- Note that the upper bound is not included in the result
sumOfMultiplesTests :: [Test]
sumOfMultiplesTests =
  [ testCase "1" $
    0 @=? sumOfMultiples [3, 5] 1
  , testCase "4" $
    3 @=? sumOfMultiples [3, 5] 4
  , testCase "10" $
    23 @=? sumOfMultiples [3, 5] 10
  , testCase "1000" $
    233168 @=? sumOfMultiples [3, 5] 1000
  , testCase "[7, 13, 17] 20" $
    51 @=? sumOfMultiples [7, 13, 17] 20
  , testCase "[4, 6] 15" $
    30 @=? sumOfMultiples [4, 6] 15
  , testCase "[5, 6, 8] 150" $
    4419 @=? sumOfMultiples [5, 6, 8] 150
  , testCase "[43, 47] 10000" $
    2203160 @=? sumOfMultiples [43, 47] 10000
  , testCase "[5, 25] 51" $
    275 @=? sumOfMultiples [5,25] 51
  , testCase "[1] 100" $
    4950 @=? sumOfMultiples [1] 100
  , testCase "[] 10000" $
    0 @=? sumOfMultiples [] 10000
  ]
