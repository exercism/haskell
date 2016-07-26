import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import LinkedList
  ( datum
  , fromList
  , isNil
  , next
  , new
  , nil
  , reverseLinkedList
  , toList
  )

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "simple-linked-list" $ do

            -- As of 2016-07-27, there was no reference file
            -- for the test cases in `exercism/x-common`.

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

            it "reverseList" $ do
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
