import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import Sublist (Sublist(Equal, Sublist, Superlist, Unequal), sublist)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList sublistTests ]

sublistTests :: [Test]
sublistTests =
  [ testCase "empty equals empty" $ do
    Equal @=? sublist "" ""
  , testCase "empty is a sublist of anything" $ do
    Sublist @=? sublist "" "asdf"
  , testCase "anything is a superlist of empty" $ do
    Superlist @=? sublist "asdf" ""
  , testCase  "1 is not 2" $ do
    Unequal @=? sublist "1" "2"
  , testCase "compare larger equal lists" $ do
    let xs = replicate 1000 'x'
    Equal @=? sublist xs xs
  , testCase "sublist at start" $ do
    Sublist @=? sublist "123" "12345"
  , testCase "sublist in middle" $ do
    Sublist @=? sublist "432" "54321"
  , testCase "sublist at end" $ do
    Sublist @=? sublist "345" "12345"
  , testCase "partially matching sublist at start" $ do
    Sublist @=? sublist "112" "1112"
  , testCase "sublist early in huge list" $ do
    Sublist @=? sublist [3, 4, 5] [1 .. 1000000 :: Int] 
  , testCase "huge sublist not in huge list" $ do
    Unequal @=? sublist [10 .. 1000001] [1 .. 1000000 :: Int]
  , testCase "superlist at start" $ do
    Superlist @=? sublist "12345" "123"
  , testCase "superlist in middle" $ do
    Superlist @=? sublist "54321" "432"
  , testCase "superlist at end" $ do
    Superlist @=? sublist "12345" "345"
  , testCase "partially matching superlist at start" $ do
    Superlist @=? sublist "1112" "112"
  , testCase "superlist early in huge list" $ do
    Superlist @=? sublist [1 .. 1000000] [3, 4, 5 :: Int]
  , testCase "recurring values sublist" $ do
    Sublist @=? sublist "12123" "1231212321"
  , testCase "recurring values unequal" $ do
    Unequal @=? sublist "12123" "1231232321"
  ]
