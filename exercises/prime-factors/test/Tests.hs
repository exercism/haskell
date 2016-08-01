{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import PrimeFactors (primeFactors)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "prime-factors" $
          describe "primeFactors" $ for_ cases test
  where

    test (n, expected) = it explanation assertion
      where
        explanation = show n
        assertion   = primeFactors n `shouldBe` expected

    -- As of 2016-07-31, there was no reference file
    -- for the test cases in `exercism/x-common`.

    cases = [ (          1,                 [] )
            , (          2,                [2] )
            , (          3,                [3] )
            , (          4,             [2, 2] )
            , (          6,             [2, 3] )
            , (          8,          [2, 2, 2] )
            , (          9,             [3, 3] )
            , (         27,          [3, 3, 3] )
            , (        625,       [5, 5, 5, 5] )
            , (     901255,   [5, 17, 23, 461] )
            , (93819012551, [11, 9539, 894119] ) ]
