{-# LANGUAGE DeriveDataTypeable #-}

import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import qualified ListOps as L
import Control.Exception (Exception, throw, evaluate, try)
import Data.Typeable (Typeable)

data FoldlIsStrictException = FoldlIsStrictException deriving (Eq, Show, Typeable)
instance Exception FoldlIsStrictException

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList listOpsTests ]


big :: Int
big = 100000

listOpsTests :: [Test]
listOpsTests =
  [ testCase "length of empty list" $ do
    0 @=? L.length ([] :: [Int])
  , testCase "length of non-empty list" $ do
    4 @=? L.length [1 .. 4 :: Int]
  , testCase "length of large list" $ do
    big @=? L.length [1 .. big :: Int]
  , testCase "reverse of empty list" $ do
    [] @=? L.reverse ([] :: [Int])
  , testCase "reverse of non-empty list" $ do
    [100 , 99 .. 1] @=? L.reverse [1 .. 100 :: Int]
  , testCase "map of empty list" $ do
    [] @=? L.map (+1) ([] :: [Int])
  , testCase "map of non-empty list" $ do
    [2, 4 .. 8] @=? L.map (+1) [1, 3 .. 7 :: Int]
  , testCase "filter of empty list" $ do
    [] @=? L.filter undefined ([] :: [Int])
  , testCase "filter of normal list" $ do
    [1, 3] @=? L.filter odd [1 .. 4 :: Int]
  , testCase "foldl' of empty list" $ do
    0 @=? L.foldl' (+) (0 :: Int) []
  , testCase "foldl' of non-empty list" $ do
    7 @=? L.foldl' (+) (-3) [1 .. 4 :: Int]
  , testCase "foldl' of huge list" $ do
    big * (big + 1) `div` 2 @=? L.foldl' (+) 0 [1 .. big]
  , testCase "foldl' with non-commutative function" $ do
    0 @=? L.foldl' (-) 10 [1 .. 4 :: Int]
  , testCase "foldl' is not just foldr . flip" $ do
    "fdsa" @=? L.foldl' (flip (:)) [] "asdf"
  , testCase "foldl' is accumulator-strict (use seq or BangPatterns)" $ do
    r <- try . evaluate $
      L.foldl' (flip const) () [throw FoldlIsStrictException, ()]
    Left FoldlIsStrictException @=? (r :: Either FoldlIsStrictException ())
  , testCase "foldr as id" $ do
    [1 .. big] @=? L.foldr (:) [] [1 .. big]
  , testCase "foldr as append" $ do
    [1 .. big] @=? L.foldr (:) [100 .. big] [1 .. 99]
  , testCase "++ of empty lists" $ do
    [] @=? [] L.++ ([] :: [Int])
  , testCase "++ of empty and non-empty lists" $ do
    [1 .. 4] @=? [] L.++ [1 .. 4 :: Int]
  , testCase "++ of non-empty and empty lists" $ do
    [1 .. 4] @=? [1 .. 4 :: Int] L.++ []
  , testCase "++ of non-empty lists" $ do
    [1 .. 5] @=? [1 .. 3] L.++ [4, 5 :: Int]
  , testCase "++ of large lists" $ do
    [1 .. big] @=? [1 .. big `div` 2] L.++ [1 + big `div` 2 .. big]
  , testCase "concat of no lists" $ do
    [] @=? L.concat ([] :: [[Int]])
  , testCase "concat of list of lists" $ do
    [1 .. 6] @=? L.concat [[1, 2], [3], [], [4, 5, 6 :: Int]]
  , testCase "concat of large list of small lists" $ do
    [1 .. big] @=? L.concat (map (:[]) [1 .. big])
  ]
