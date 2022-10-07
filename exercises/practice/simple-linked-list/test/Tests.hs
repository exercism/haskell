{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.Hspec                (Spec, it, shouldBe)
import Test.Hspec.Runner         (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck           (property)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

import LinkedList
  ( datum
  , fromList
  , isNil
  , next
  , new
  , nil
  , reverseLinkedList
  , toList
  , LinkedList
  )

instance (Arbitrary a) => Arbitrary (LinkedList a) where
  arbitrary = fromList <$> arbitrary

nthDatum :: LinkedList a -> Int -> a
nthDatum xs 0 = datum xs
nthDatum xs n = nthDatum (next xs) (pred n)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

            let n1   = new (1 :: Int) nil
            let n21  = new 2 n1
            let n321 = new 3 n21
            let fl1  = fromList [1 :: Int]
            let fl21 = fromList [2, 1 :: Int]
            let r1   = reverseLinkedList n1
            let r12  = reverseLinkedList n21
            let r123 = reverseLinkedList n321
            let msg  = "Should work for any type, not just Int!"

            it "constructor" $ do
                isNil nil               `shouldBe` True
                isNil n1                `shouldBe` False
                datum n1                `shouldBe` 1
                isNil (next n1)         `shouldBe` True
                isNil n21               `shouldBe` False
                datum n21               `shouldBe` 2
                isNil (next n21)        `shouldBe` False
                datum (next n21)        `shouldBe` 1
                isNil (next $ next n21) `shouldBe` True

            it "toList" $ do
                toList nil `shouldBe` ([] :: [Int])
                toList n1  `shouldBe` [1]
                toList n21 `shouldBe` [2, 1]

            it "fromList" $ do
                isNil (fromList [])      `shouldBe` True
                datum fl1                `shouldBe` 1
                isNil (next fl1)         `shouldBe` True
                datum fl21               `shouldBe` 2
                datum (next fl21)        `shouldBe` 1
                isNil (next $ next fl21) `shouldBe` True

            it "reverseLinkedList" $ do
                isNil (reverseLinkedList nil) `shouldBe` True
                datum r1                      `shouldBe` 1
                isNil (next r1)               `shouldBe` True
                datum r12                     `shouldBe` 1
                datum (next r12)              `shouldBe` 2
                isNil (next $ next r12)       `shouldBe` True
                datum r123                    `shouldBe` 1
                datum (next r123)             `shouldBe` 2
                datum (next $ next r123)      `shouldBe` 3

            it "roundtrip" $ do
                (toList . fromList) []      `shouldBe` ([]      :: [Int])
                (toList . fromList) [1]     `shouldBe` ([1]     :: [Int])
                (toList . fromList) [1, 2]  `shouldBe` ([1, 2]  :: [Int])
                (toList . fromList) [1..10] `shouldBe` ([1..10] :: [Int])

            it "has an unconstrained type variable" $ do
                (toList . fromList) msg     `shouldBe` msg
                (toList . fromList) [1..10] `shouldBe` ([1..10] :: [Integer])

            it "arbitrary reverseLinkedList" $
                property $ \(xs :: LinkedList Int) ->
                  reverseLinkedList xs == (fromList . reverse . toList) xs

            it "arbitrary (fromList . toList)" $
                property $ \(xs :: LinkedList Int) ->
                  (fromList . toList) xs == xs

            it "arbitrary (toList . fromList)" $
                property $ \(xs :: [Int]) ->
                  (toList . fromList) xs == xs

            it "arbitrary (reverseLinkedList . reverseLinkedList)" $
                property $ \(xs :: LinkedList Int) ->
                  (reverseLinkedList . reverseLinkedList) xs == xs

            it "arbitrary datum" $
                property $ \(xs :: [Int]) ->
                  let ll = fromList xs
                      sameNthDatum n = nthDatum ll n == xs !! n
                      indices = [0..pred $ length xs] in
                    all sameNthDatum indices
