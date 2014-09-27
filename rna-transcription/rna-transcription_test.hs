import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import DNA (toRNA)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

toRNATests :: [Test]
toRNATests =
  [ testCase "transcribes cytosine to guanine" $
    "G" @=? toRNA "C"
  , testCase "transcribes guanine to cytosine" $
    "C" @=? toRNA "G"
  , testCase "transcribes adenine to uracil" $
    "U" @=? toRNA "A"
  , testCase "transcribes thymine to adenine" $
    "A" @=? toRNA "T"
  , testCase "transcribes all ACGT to UGCA" $
    "UGCACCAGAAUU" @=? toRNA "ACGTGGTCTTAA"
  ]

main :: IO ()
main = exitProperly (runTestTT (TestList toRNATests))
