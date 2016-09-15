{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Series (slices)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "series" $ do

    -- As of 2016-09-12, there was no reference file
    -- for the test cases in `exercism/x-common`.

    it "slices of one" $ do
      slices 1 ""      `shouldBe` []
      slices 1 "01234" `shouldBe` [[0], [1], [2], [3], [4]]

    it "slices of two" $ do
      slices 2 ""      `shouldBe` []
      slices 2 "01"    `shouldBe` [[0,1]]
      slices 2 "01234" `shouldBe` [[0,1], [1,2], [2,3], [3,4]]

    it "slices of three" $ do
      slices 3 "ab"   `shouldBe` []
      slices 3 "012"  `shouldBe` [[0,1,2]]
      slices 3 "0123" `shouldBe` [[0,1,2], [1,2,3]]

    it "slices of zero" $ do
      slices 0 ""    `shouldBe` [[]]
      slices 0 "012" `shouldBe` [[]]
