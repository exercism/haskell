{-# LANGUAGE DeriveAnyClass #-}

import Control.Exception (Exception, throw, evaluate)
import Test.Hspec        (Spec, it, shouldBe, shouldThrow)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Prelude hiding
    ( (++)
    , concat
    , filter
    , foldr
    , length
    , map
    , reverse
    )

import ListOps
    ( (++)
    , concat
    , filter
    , foldl'
    , foldr
    , length
    , map
    , reverse
    )

data StrictException = StrictException deriving (Eq, Show, Exception)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    let big = 100000 :: Int

    it "length of empty list" $
      length ([] :: [Int]) `shouldBe` 0

    it "length of non-empty list" $
      length [1 .. 4 :: Int] `shouldBe` 4

    it "length of large list" $
      length [1 .. big :: Int] `shouldBe` big

    it "reverse of empty list" $
      reverse ([] :: [Int]) `shouldBe` []

    it "reverse of non-empty list" $
      reverse [1 .. 100 :: Int] `shouldBe` [100 , 99 .. 1]

    it "map of empty list" $
      map (+1) ([] :: [Int]) `shouldBe` []

    it "map of non-empty list" $
      map (+1) [1, 3 .. 7 :: Int] `shouldBe` [2, 4 .. 8]

    it "filter of empty list" $
      filter undefined ([] :: [Int]) `shouldBe` []

    it "filter of normal list" $
      filter odd [1 .. 4 :: Int] `shouldBe` [1, 3]

    it "foldl' of empty list" $
      foldl' (+) (0 :: Int) [] `shouldBe` 0

    it "foldl' of non-empty list" $
      foldl' (+) (-3) [1 .. 4 :: Int] `shouldBe` 7

    it "foldl' of huge list" $
      foldl' (+) 0 [1 .. big] `shouldBe` big * (big + 1) `div` 2

    it "foldl' with non-commutative function" $
      foldl' (-) 10 [1 .. 4 :: Int] `shouldBe` 0

    it "foldl' is not just foldr . flip" $
      foldl' (flip (:)) [] "asdf" `shouldBe` "fdsa"

    it "foldl' is accumulator-strict (use seq or BangPatterns)" $
      evaluate (foldl' (flip const) () [throw StrictException, ()])
      `shouldThrow` (== StrictException)

    it "foldr as id" $
      foldr (:) [] [1 .. big] `shouldBe` [1 .. big]

    it "foldr as append" $
      foldr (:) [100 .. big] [1 .. 99] `shouldBe` [1 .. big]

    it "++ of empty lists" $
      [] ++ ([] :: [Int]) `shouldBe` []

    it "++ of empty and non-empty lists" $
      [] ++ [1 .. 4 :: Int] `shouldBe` [1 .. 4]

    it "++ of non-empty and empty lists" $
      [1 .. 4 :: Int] ++ [] `shouldBe` [1 .. 4]

    it "++ of non-empty lists" $
      [1 .. 3] ++ [4, 5 :: Int] `shouldBe` [1 .. 5]

    it "++ of large lists" $
      [1 .. big `div` 2] ++ [1 + big `div` 2 .. big] `shouldBe` [1 .. big]

    it "concat of no lists" $
      concat ([] :: [[Int]]) `shouldBe` []

    it "concat of list of lists" $
      concat [[1, 2], [3], [], [4, 5, 6 :: Int]] `shouldBe` [1 .. 6]

    it "concat of large list of small lists" $
      concat (map (:[]) [1 .. big]) `shouldBe` [1 .. big]
