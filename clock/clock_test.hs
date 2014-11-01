import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import Clock (fromHourMin, toString)

-- Clock should define a data type that is an instance of Eq and
-- Num such that the `fromInteger` converts minutes
-- to 24 hour clock time. It is not necessary to have
-- a sensible implementation of `abs` or `signum`.

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList clockTests ]

clockTests :: [Test]
clockTests =
  [ testCase "test constructor and toString" $ do
    "08:00" @=? toString (fromHourMin 8 0)
    "09:00" @=? toString (fromHourMin 9 0)
    "11:09" @=? toString (fromHourMin 11 9)
  , testCase "fromInteger should work in minutes" $ do
    "00:03" @=? toString 3
  , testCase "constructor and fromInteger should be compatible" $ do
    fromHourMin 1 0 @=? 60
  , testCase "adding clocks" $ do
    "10:03" @=? toString (fromHourMin 10 0 + 3)
    "11:01" @=? toString (fromHourMin 10 0 + 61)
    "00:30" @=? toString (fromHourMin 23 30 + 60)
  , testCase "subtracting clocks" $ do
    "08:30" @=? toString (fromHourMin 10 0 - 90)
    "23:30" @=? toString (fromHourMin 0 30 - 60)
  , testCase "clock implements Eq" $ do
    fromHourMin 15 37 @=? fromHourMin 15 37
    False @=? (fromHourMin 15 37 == fromHourMin 15 36)
    True @=? (fromHourMin 15 37 /= fromHourMin 15 36)
    True @=? (fromHourMin 15 37 /= fromHourMin 14 37)
  , testCase "Eq respects wraparound" $ do
    fromHourMin 24 0 @=? fromHourMin 0 0
    fromHourMin 25 0 @=? fromHourMin 1 0
  , testCase "negate works" $ do
    5 @=? negate (fromHourMin 23 55)
  ]
