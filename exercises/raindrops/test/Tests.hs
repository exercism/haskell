{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Raindrops (convert)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "raindrops" $
          describe "convert" $ for_ cases test
  where

    test (number, expected) = it description assertion
      where
        description = show number
        assertion   = convert number `shouldBe` expected

    -- Test cases adapted from `exercism/x-common/raindrops.json` on 2016-08-01.

    cases = [ (  1, "1"              )
            , (  3, "Pling"          )
            , (  5, "Plang"          )
            , (  7, "Plong"          )
            , (  6, "Pling"          )
            , (  9, "Pling"          )
            , ( 10, "Plang"          )
            , ( 14, "Plong"          )
            , ( 15, "PlingPlang"     )
            , ( 21, "PlingPlong"     )
            , ( 25, "Plang"          )
            , ( 35, "PlangPlong"     )
            , ( 49, "Plong"          )
            , ( 52, "52"             )
            , (105, "PlingPlangPlong") ]
