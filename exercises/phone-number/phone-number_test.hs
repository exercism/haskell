import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import Phone (areaCode, number, prettyPrint)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList numberTests
       , TestList areaCodeTests
       , TestList prettyPrintTests ]

numberTests :: [Test]
numberTests =
  [ testCase "cleans number" $
    "1234567890" @=? number "(123) 456-7890"
  , testCase "cleans another number" $
    "6125551212" @=? number "(612) 555-1212"
  , testCase "cleans number with dots" $
    "1234567890" @=? number "123.456.7890"
  , testCase "cleans another number with dots" $
    "9187654321" @=? number "918.765.4321"
  , testCase "valid when 11 digits and first is 1" $
    "2468013579" @=? number "12468013579"
  , testCase "invalid when 11 digits" $
    "0000000000" @=? number "21234567890"
  , testCase "invalid when 9 digits" $
    "0000000000" @=? number "123456789"
  , testCase "invalid when 12 digits" $
    "0000000000" @=? number "123456789012"
  , testCase "invalid when empty" $
    "0000000000" @=? number ""
  , testCase "invalid when no digits present" $
    "0000000000" @=? number " (-) "
  , testCase "valid with leading characters" $
    "2358132134" @=? number "my number is 235 813 2134"
  , testCase "valid with trailing characters" $
    "9876543210" @=? number "987 654 3210 - bob"
  , testCase "valid amidst text and punctuation" $
    "4158880000" @=? number "Here it is: 415-888-0000. Thanks!"
  ]

areaCodeTests :: [Test]
areaCodeTests =
  [ testCase "area code" $
    "123" @=? areaCode "1234567890"
  , testCase "area code with parentheses" $
    "612" @=? areaCode "(612) 555-1212"
  , testCase "area code with leading characters" $
    "235" @=? areaCode "my number is 235 813 2134"
  , testCase "invalid area code" $
    "000" @=? areaCode " (-) "
  ]

prettyPrintTests :: [Test]
prettyPrintTests =
  [ testCase "pretty print" $
    "(123) 456-7890" @=? prettyPrint "1234567890"
  , testCase "pretty print with full US phone number" $
    "(234) 567-8901" @=? prettyPrint "12345678901"
  , testCase "pretty print amidst text and punctuation" $
    "(415) 888-0000" @=? prettyPrint "Here it is: 415-888-0000. Thanks!"
  ]
