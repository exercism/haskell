import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import DNA (count, nucleotideCounts)
import Data.Map (fromList)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList countTests
       , TestList nucleotideCountTests]

countTests :: [Test]
countTests =
  [ testCase "empty dna strand has no adenosine" $
    Right 0 @=? count 'A' ""
  , testCase "repetitive cytidine gets counted" $
    Right 5 @=? count 'C' "CCCCC"
  , testCase "counts only thymidine" $
    Right 1 @=? count 'T' "GGGGGTAACCCGG"
  , testCase "validates nucleotides" $
    Left "invalid nucleotide 'X'" @=? count 'X' "GACT"
  , testCase "validates strand" $
    Left "invalid nucleotide 'Y'" @=? count 'G' "GACYT"
  ]

nucleotideCountTests :: [Test]
nucleotideCountTests =
  [ testCase "empty dna strand has no nucleotides" $
    Right (fromList [('A', 0), ('T', 0), ('C', 0), ('G', 0)]) @=?
    nucleotideCounts ""
  , testCase "repetitive-sequence-has-only-guanosine" $
    Right (fromList [('A', 0), ('T', 0), ('C', 0), ('G', 8)]) @=?
    nucleotideCounts "GGGGGGGG"
  , testCase "counts all nucleotides" $
    Right (fromList [('A', 20), ('T', 21), ('C', 12), ('G', 17)]) @=?
    nucleotideCounts ("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAA" ++
                      "GAGTGTCTGATAGCAGC")
  , testCase "validates strand" $
    Left "invalid nucleotide 'P'" @=? nucleotideCounts "GPAC"
  ]
