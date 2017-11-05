{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.List         (sort)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Prelude hiding (null)

import CustomSet
  ( delete
  , difference
  , fromList
  , insert
  , isDisjointFrom
  , isSubsetOf
  , intersection
  , member
  , null
  , size
  , toList
  , union
  )

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    describe "standard tests" $ do

      describe "null" $ do

        it "sets with no elements are empty" $
          null (fromList ([] :: [Integer])) `shouldBe` True

        it "sets with elements are not empty" $
          null (fromList [1]) `shouldBe` False

      describe "member" $ do

        it "nothing is contained in an empty set" $
          1 `member` fromList [] `shouldBe` False

        it "when the element is in the set" $
          1 `member` fromList [1, 2, 3] `shouldBe` True

        it "when the element is not in the set" $
          4 `member` fromList [1, 2, 3] `shouldBe` False

      describe "isSubsetOf" $ do

        it "empty set is a subset of another empty set" $
          fromList ([] :: [Integer]) `isSubsetOf` fromList [] `shouldBe` True

        it "empty set is a subset of non-empty set" $
          fromList [] `isSubsetOf` fromList [1] `shouldBe` True

        it "non-empty set is not a subset of empty set" $
          fromList [1] `isSubsetOf` fromList [] `shouldBe` False

        it "set is a subset of set with exact same elements" $
          fromList [1, 2, 3] `isSubsetOf` fromList [1, 2, 3] `shouldBe` True

        it "set is a subset of larger set with same elements" $
          fromList [1, 2, 3] `isSubsetOf` fromList [4, 1, 2, 3] `shouldBe` True

        it "set is not a subset of set that does not contain its elements" $
          fromList [1, 2, 3] `isSubsetOf` fromList [4, 1, 3] `shouldBe` False

      describe "isDisjointFrom" $ do

        it "the empty set is disjoint with itself" $
          fromList ([] :: [Integer]) `isDisjointFrom` fromList [] `shouldBe` True

        it "empty set is disjoint with non-empty set" $
          fromList [] `isDisjointFrom` fromList [1] `shouldBe` True

        it "non-empty set is disjoint with empty set" $
          fromList [1] `isDisjointFrom` fromList [] `shouldBe` True

        it "sets are not disjoint if they share an element" $
          fromList [1, 2] `isDisjointFrom` fromList [2, 3] `shouldBe` False

        it "sets are disjoint if they share no elements" $
          fromList [1, 2] `isDisjointFrom` fromList [3, 4] `shouldBe` True

      describe "Eq" $ do

        it "empty sets are equal" $
          (fromList ([] :: [Integer]) == fromList []) `shouldBe` True

        it "empty set is not equal to non-empty set" $
          (fromList [] == fromList [1, 2, 3]) `shouldBe` False

        it "non-empty set is not equal to empty set" $
          (fromList [1, 2, 3] == fromList []) `shouldBe` False

        it "sets with the same elements are equal" $
          (fromList [1, 2] == fromList [2, 1]) `shouldBe` True

        it "sets with different elements are not equal" $
          (fromList [1, 2, 3] == fromList [1, 2, 4]) `shouldBe` False

        it "set is not equal to larger set with same elements" $
          (fromList [1, 2, 3] == fromList [1, 2, 3, 4]) `shouldBe` False

      describe "insert" $ do

        it "add to empty set" $
          insert 3 (fromList []) `shouldBe` fromList [3]

        it "add to non-empty set" $
          insert 3 (fromList [1, 2, 4]) `shouldBe` fromList [1, 2, 3, 4]

        it "adding an existing element does not change the set" $
          insert 3 (fromList [1, 2, 3]) `shouldBe` fromList [1, 2, 3]

      describe "intersection" $ do

        it "intersection of two empty sets is an empty set" $
          fromList ([] :: [Integer]) `intersection` fromList [] `shouldBe` fromList []

        it "intersection of an empty set and non-empty set is an empty set" $
          fromList [] `intersection` fromList [3, 2, 5] `shouldBe` fromList []

        it "intersection of a non-empty set and an empty set is an empty set" $
          fromList [1, 2, 3, 4] `intersection` fromList [] `shouldBe` fromList []

        it "intersection of two sets with no shared elements is an empty set" $
          fromList [1, 2, 3] `intersection` fromList [4, 5, 6] `shouldBe` fromList []

        it "intersection of two sets with shared elements is a set of the shared elements" $
          fromList [1, 2, 3, 4] `intersection` fromList [3, 2, 5] `shouldBe` fromList [2, 3]

      describe "difference" $ do

        it "difference of two empty sets is an empty set" $
          fromList ([] :: [Integer]) `difference` fromList [] `shouldBe` fromList []

        it "difference of empty set and non-empty set is an empty set" $
          fromList [] `difference` fromList [3, 2, 5] `shouldBe` fromList []

        it "difference of a non-empty set and an empty set is the non-empty set" $
          fromList [1, 2, 3, 4] `difference` fromList [] `shouldBe` fromList [1, 2, 3, 4]

        it "difference of two non-empty sets is a set of elements that are only in the first set" $
          fromList [3, 2, 1] `difference` fromList [2, 4] `shouldBe` fromList [1, 3]

      describe "union" $ do

        it "union of empty sets is an empty set" $
          fromList ([] :: [Integer]) `union` fromList [] `shouldBe` fromList []

        it "union of an empty set and non-empty set is the non-empty set" $
          fromList [] `union` fromList [2] `shouldBe` fromList [2]

        it "union of a non-empty set and empty set is the non-empty set" $
          fromList [1, 3] `union` fromList [] `shouldBe` fromList [1, 3]

        it "union of non-empty sets contains all unique elements" $
          fromList [1, 3] `union` fromList [2, 3] `shouldBe` fromList [3, 2, 1]

    describe "track-specific tests" $ do

      -- Track-specific test cases.

      describe "delete" $

        it "delete existing element" $
          delete 2 (fromList [1, 2, 3]) `shouldBe` fromList [1, 3]

      describe "size" $ do

        it "size of an empty set is zero" $
          size (fromList ([] :: [Integer])) `shouldBe` 0

        it "size is the number of elements" $
          size (fromList [1, 2, 3]) `shouldBe` 3

        it "size doesn't count repetition in `fromList`" $
          size (fromList [1, 2, 3, 2]) `shouldBe` 3

        it "size does count inserted element" $
          size (insert 3 (fromList [1, 2])) `shouldBe` 3

        it "size doesn't count element removed by `delete`" $
          size (delete 3 (fromList [1, 2, 3, 4])) `shouldBe` 3

      describe "toList" $ do

        it "an empty set has an empty list of elements" $
          (sort . toList . fromList) ([] :: [Integer]) `shouldBe `[]

        it "a set has the list of its elements" $
          (sort . toList . fromList) [3, 1, 2] `shouldBe `[1, 2, 3]

        it "a set doesn't keep repeated elements" $
          (sort . toList . fromList) [3, 1, 2, 1] `shouldBe `[1, 2, 3]
