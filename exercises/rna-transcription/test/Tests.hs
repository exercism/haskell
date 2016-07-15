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
    Just "G" @=? toRNA "C"
  , testCase "transcribes guanine to cytosine" $
    Just "C" @=? toRNA "G"
  , testCase "transcribes adenine to uracil" $
    Just "U" @=? toRNA "A"
  , testCase "transcribes thymine to adenine" $
    Just "A" @=? toRNA "T"
  , testCase "transcribes all ACGT to UGCA" $
    Just "UGCACCAGAAUU" @=? toRNA "ACGTGGTCTTAA"
  , testCase "transcribes RNA only nucleotide uracil to Nothing" $
    Nothing @=? toRNA "U"
  , testCase "transcribes completely invalid DNA to Nothing" $
    Nothing @=? toRNA "XXX"
  , testCase "transcribes partially invalid DNA to Nothing" $
    Nothing @=? toRNA "ACGTXXXCTTAA"
  ]

main :: IO ()
main = exitProperly (runTestTT (TestList toRNATests))
