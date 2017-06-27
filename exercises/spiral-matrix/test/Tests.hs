{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Spiral (spiral)

import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "spiral" $ do

  it "empty spiral" $
    spiral 0 `shouldBe` []

  it "trivial spiral" $
    spiral 1 `shouldBe` [ [1] ]

  it "spiral of side length 2" $
    spiral 2 `shouldBe` [ [1, 2]
                        , [4, 3] ]

  it "spiral of side length 3" $
    spiral 3 `shouldBe` [ [1, 2, 3]
                        , [8, 9, 4]
                        , [7, 6, 5] ]

  it "spiral of side length 4" $
    spiral 4 `shouldBe` [ [ 1,  2,  3, 4]
                        , [12, 13, 14, 5]
                        , [11, 16, 15, 6]
                        , [10,  9,  8, 7] ]

  it "spiral of side length 5" $
    spiral 5 `shouldBe` [ [ 1,  2,  3,  4, 5]
                        , [16, 17, 18, 19, 6]
                        , [15, 24, 25, 20, 7]
                        , [14, 23, 22, 21, 8]
                        , [13, 12, 11, 10, 9] ]
