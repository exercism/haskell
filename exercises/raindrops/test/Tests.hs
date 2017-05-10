{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Raindrops (convert)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "convert" $ for_ cases test
  where

    test (number, expected) = it description assertion
      where
        description = show number
        assertion   = convert number `shouldBe` expected

    cases = [ (   1, "1"              )
            , (   3, "Pling"          )
            , (   5, "Plang"          )
            , (   7, "Plong"          )
            , (   6, "Pling"          )
            , (   8, "8"              )
            , (   9, "Pling"          )
            , (  10, "Plang"          )
            , (  14, "Plong"          )
            , (  15, "PlingPlang"     )
            , (  21, "PlingPlong"     )
            , (  25, "Plang"          )
            , (  27, "Pling"          )
            , (  35, "PlangPlong"     )
            , (  49, "Plong"          )
            , (  52, "52"             )
            , ( 105, "PlingPlangPlong")
            , (3125, "Plang"          ) ]
