import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import Series (largestProduct)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
  [ TestList seriesTests ]

-- Allow implementations that work with Integral to compile without warning
int :: Int -> Maybe Int
int = Just

intNothing :: Maybe Int
intNothing = Nothing

seriesTests :: [Test]
seriesTests = map TestCase
  [ int 72 @=? largestProduct 2 "0123456789"
  , int 2 @=? largestProduct 2 "12"
  , int 9 @=? largestProduct 2 "19"
  , int 48 @=? largestProduct 2 "576802143"
  , int 504 @=? largestProduct 3 ['0'..'9']
  , int 270 @=? largestProduct 3 "1027839564"
  , int 15120 @=? largestProduct 5 ['0'..'9']
  , int 23520 @=?
    largestProduct 6 "73167176531330624919225119674426574742355349194934"
  , int 28350 @=?
    largestProduct 6 "52677741234314237566414902593461595376319419139427"
  , int 1 @=? largestProduct 0 ""
  , int 1 @=? largestProduct 0 "123"
  , intNothing @=? largestProduct 1 ""
  , intNothing @=? largestProduct 4 "123"
    -- if all spans contain zero, result is zero.
  , int 0 @=? largestProduct 3 "99099"
  , int 0 @=? largestProduct 2 "00"
  ]
