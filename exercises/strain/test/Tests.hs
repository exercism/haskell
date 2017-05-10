import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Data.List (isPrefixOf)

import Strain (discard, keep)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    it "empty keep" $
        keep (<10) [] `shouldBe` ([] :: [Int])

    it "keep everything" $
        keep (<10) [1, 2, 3] `shouldBe` [1, 2, 3 :: Int]

    it "keep first and last" $
        keep odd [1, 2, 3] `shouldBe` [1, 3 :: Int]

    it "keep nothing" $
        keep even [1, 3, 5, 7] `shouldBe` ([] :: [Int])

    it "keep neither first nor last" $
        keep even [1, 2, 3] `shouldBe` [2 :: Int]

    it "keep strings" $
        keep ("z" `isPrefixOf`)
        ["apple", "zebra", "banana", "zombies", "cherimoya", "zealot"]
        `shouldBe`
        ["zebra", "zombies", "zealot"]

    it "empty discard" $
        discard (< 10) [] `shouldBe` ([] :: [Int])

    it "discard everything" $
        discard (< 10) [1, 2, 3] `shouldBe` ([] :: [Int])

    it "discard first and last" $
        discard odd [1, 2, 3] `shouldBe` [2 :: Int]

    it "discard nothing" $
        discard even [1, 3, 5, 7] `shouldBe` [1, 3, 5, 7 :: Int]

    it "discard neither first nor last" $
        discard even [1, 2, 3] `shouldBe` [1, 3 :: Int]

    it "discard strings" $
        discard ("z" `isPrefixOf`)
        ["apple", "zebra", "banana", "zombies", "cherimoya", "zealot"]
        `shouldBe`
        ["apple", "banana", "cherimoya"]

    it "keep non-strict" $
        (take 1 . keep (const True))
        ("yes" : error "keep should be lazier - don't look at list elements you don't need!")
        `shouldBe`
        ["yes"]

    it "discard non-strict" $
        (take 1 . discard (const False))
        ("yes" : error "discard should be lazier - don't look at list elements you don't need!")
        `shouldBe`
        ["yes"]
