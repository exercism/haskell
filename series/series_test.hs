import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import Series (slices)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList seriesTests ]

seriesTests :: [Test]
seriesTests =
  [ testCase "slices of one" $ do
    [] @=? slices 1 ""
    [[0],[1],[2],[3],[4]] @=? slices 1 "01234"
  , testCase "slices of two" $ do
    [] @=? slices 2 ""
    [[0,1]] @=? slices 2 "01"
    [[0,1],[1,2],[2,3],[3,4]] @=? slices 2 "01234"
  , testCase "slices of three" $ do
    [] @=? slices 3 "ab"
    [[0,1,2]] @=? slices 3 "012"
    [[0,1,2],[1,2,3]] @=? slices 3 "0123"
  ]
