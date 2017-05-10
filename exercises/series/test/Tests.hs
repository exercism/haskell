{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}

import GHC.Exts          (toList)
import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Series (slices)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    let x `shouldHaveSlices` yss = (map toList . toList) x `shouldBe` yss

    it "slices of one" $ do
      slices 1 ""      `shouldHaveSlices` []
      slices 1 "01234" `shouldHaveSlices` [[0], [1], [2], [3], [4]]

    it "slices of two" $ do
      slices 2 ""      `shouldHaveSlices` []
      slices 2 "01"    `shouldHaveSlices` [[0,1]]
      slices 2 "01234" `shouldHaveSlices` [[0,1], [1,2], [2,3], [3,4]]

    it "slices of three" $ do
      slices 3 "ab"   `shouldHaveSlices` []
      slices 3 "012"  `shouldHaveSlices` [[0,1,2]]
      slices 3 "0123" `shouldHaveSlices` [[0,1,2], [1,2,3]]

    it "slices of zero" $ do
      slices 0 ""    `shouldHaveSlices` [[]]
      slices 0 "012" `shouldHaveSlices` [[]]
