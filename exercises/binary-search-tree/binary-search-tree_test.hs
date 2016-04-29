import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import BST (bstLeft, bstRight, bstValue, empty, singleton, insert, fromList, toList)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList bstTests ]

int4 :: Int
int4 = 4

noInts :: [Int]
noInts = []

bstTests :: [Test]
bstTests =
  [ testCase "data is retained" $
    Just 4 @=? bstValue (singleton int4)
  , testCase "inserting less" $ do
    let t = insert 2 (singleton int4)
    Just 4 @=? bstValue t
    Just 2 @=? (bstLeft t >>= bstValue)
  , testCase "inserting same" $ do
    let t = insert 4 (singleton int4)
    Just 4 @=? bstValue t
    Just 4 @=? (bstLeft t >>= bstValue)
  , testCase "inserting right" $ do
    let t = insert 5 (singleton int4)
    Just 4 @=? bstValue t
    Just 5 @=? (bstRight t >>= bstValue)
  , testCase "empty list to tree" $
    empty @=? fromList noInts
  , testCase "empty list has no value" $
    Nothing @=? bstValue (fromList noInts)
  , testCase "inserting into empty" $ do
    let t = insert int4 empty
    Just 4 @=? bstValue t
  , testCase "complex tree" $ do
    let t = fromList [int4, 2, 6, 1, 3, 7, 5]
    Just 4 @=? bstValue t
    Just 2 @=? (bstLeft t >>= bstValue)
    Just 1 @=? (bstLeft t >>= bstLeft >>= bstValue)
    Just 3 @=? (bstLeft t >>= bstRight >>= bstValue)
    Just 6 @=? (bstRight t >>= bstValue)
    Just 5 @=? (bstRight t >>= bstLeft >>= bstValue)
    Just 7 @=? (bstRight t >>= bstRight >>= bstValue)
  , testCase "empty tree to list" $
    0 @=? length (toList empty)
  , testCase "iterating one element" $
    [4] @=? toList (singleton int4)
  , testCase "iterating over smaller element" $
    [2, 4] @=? toList (fromList [int4, 2])
  , testCase "iterating over larger element" $
    [4, 5] @=? toList (fromList [int4, 5])
  , testCase "iterating over complex tree" $
    [1..7] @=? toList (fromList [int4, 2, 1, 3, 6, 7, 5])
  ]