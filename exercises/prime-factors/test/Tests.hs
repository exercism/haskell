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

    -- Test cases adapted from `exercism/x-common/prime-factors` on 2017-02-01.

    cases = [ (          1,                 [] )
            , (          2,                [2] )
            , (          9,             [3, 3] )
            , (          8,          [2, 2, 2] )
            , (         12,          [2, 2, 3] )
            , (     901255,   [5, 17, 23, 461] )
            , (93819012551, [11, 9539, 894119] ) ]
