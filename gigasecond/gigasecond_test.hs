import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import Gigasecond (fromDay)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (readTime)
import System.Locale (defaultTimeLocale, iso8601DateFormat)

dt :: String -> UTCTime
dt = readTime defaultTimeLocale (iso8601DateFormat (Just "%T%Z"))

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList gigasecondTests ]

gigasecondTests :: [Test]
gigasecondTests =
  [ testCase "from apr 25 2011" $
    dt "2043-01-01T01:46:40Z" @=? fromDay (dt "2011-04-25T00:00:00Z")
  , testCase "from jun 13 1977" $
    dt "2009-02-19T01:46:40Z" @=? fromDay (dt "1977-06-13T00:00:00Z")
  , testCase "from jul 19 1959" $
    dt "1991-03-27T01:46:40Z" @=? fromDay (dt "1959-07-19T00:00:00Z")
    -- customize this to test your birthday and find your gigasecond date:
  ]
