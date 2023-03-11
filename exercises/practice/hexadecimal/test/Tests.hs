{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Hexadecimal (hexToInt)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "hexToInt" $ for_ cases test
  where

    test (input, expected) = it description assertion
      where
        description = show input
        assertion   = hexToInt input `shouldBe` expected

    cases = [ (     "1",        1)
            , (     "c",       12)
            , (    "10",       16)
            , (    "af",      175)
            , (   "100",      256)
            , ("19ace" ,   105166)
            , ("carrot",        0)
            , ("000000",        0)
            , ("ffffff", 16777215)
            , ("ffff00", 16776960) ]
