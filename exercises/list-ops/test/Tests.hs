{-# LANGUAGE DeriveAnyClass #-}

import Control.Exception (Exception, throw, evaluate)
import Test.Hspec        (Spec, describe, it, shouldBe, shouldThrow)
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

    describe "length" $ do
      it "of empty list" $
        length ([] :: [Int]) `shouldBe` 0
      it "of non-empty list" $
        length [1 .. 4 :: Int] `shouldBe` 4
      -- Track-specific test
      it "of large list" $
        length [1 .. big :: Int] `shouldBe` big

    describe "reverse" $ do
      it "of empty list" $
        reverse ([] :: [Int]) `shouldBe` []
      it "of non-empty list" $
        reverse [1 .. 100 :: Int] `shouldBe` [100 , 99 .. 1]
      it "of nested lists" $
        reverse [[1, 2], [3], [], [4, 5, 6] :: [Int]] `shouldBe` [[4, 5, 6], [], [3], [1, 2]]

    describe "map" $ do
      it "of empty list" $
        map (+1) ([] :: [Int]) `shouldBe` []
      it "of non-empty list" $
        map (+1) [1, 3 .. 7 :: Int] `shouldBe` [2, 4 .. 8]

    describe "filter" $ do
      it "of empty list" $
        filter undefined ([] :: [Int]) `shouldBe` []
      it "of normal list" $
        filter odd [1 .. 4 :: Int] `shouldBe` [1, 3]

    describe "foldl'" $ do
      it "of empty list" $
        foldl' (+) (0 :: Int) [] `shouldBe` 0
      it "of non-empty list" $
        foldl' (+) (-3) [1 .. 4 :: Int] `shouldBe` 7
      -- Track-specific test
      it "of huge list" $
        foldl' (+) 0 [1 .. big] `shouldBe` big * (big + 1) `div` 2
      it "with non-commutative function" $
        foldl' (-) 10 [1 .. 4 :: Int] `shouldBe` 0
      -- Track-specific test
      it "is not just foldr . flip" $
        foldl' (flip (:)) [] "asdf" `shouldBe` "fdsa"
      -- Track-specific test
      it "is accumulator-strict (use seq or BangPatterns)" $
        evaluate (foldl' (flip const) () [throw StrictException, ()])
        `shouldThrow` (== StrictException)

    describe "foldr" $ do
      it "of empty list" $
        foldr (*) (2 :: Int) [] `shouldBe` 2
      it "of non-empty list" $
        foldr (+) 5 [1 .. 4 :: Int] `shouldBe` 15
      it "with non-commutative function" $
        foldr div 5 [2, 5 :: Int] `shouldBe` 2
      -- Track-specific test
      it "as id" $
        foldr (:) [] [1 .. big] `shouldBe` [1 .. big]
      -- Track-specific test
      it "as append" $
        foldr (:) [100 .. big] [1 .. 99] `shouldBe` [1 .. big]

    describe "++" $ do
      it "of empty lists" $
        [] ++ ([] :: [Int]) `shouldBe` []
      it "of empty and non-empty lists" $
        [] ++ [1 .. 4 :: Int] `shouldBe` [1 .. 4]
      it "of non-empty and empty lists" $
        [1 .. 4 :: Int] ++ [] `shouldBe` [1 .. 4]
      it "of non-empty lists" $
        [1 .. 3] ++ [4, 5 :: Int] `shouldBe` [1 .. 5]
      -- Track-specific test
      it "of large lists" $
        [1 .. big `div` 2] ++ [1 + big `div` 2 .. big] `shouldBe` [1 .. big]

    describe "concat" $ do
      it "of no lists" $
        concat ([] :: [[Int]]) `shouldBe` []
      it "of list of lists" $
        concat [[1, 2], [3], [], [4, 5, 6 :: Int]] `shouldBe` [1 .. 6]
      it "of list of nested lists" $
        concat [[[1], [2]], [[3]], [[]], [[4, 5, 6 :: Int]]] `shouldBe` [[1], [2], [3], [], [4, 5, 6 :: Int]]
      -- Track-specific test
      it "of large list of small lists" $
        concat (map (:[]) [1 .. big]) `shouldBe` [1 .. big]
