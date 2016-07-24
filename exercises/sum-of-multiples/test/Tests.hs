{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import SumOfMultiples (sumOfMultiples)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "sum-of-multiples" $
          describe "sumOfMultiples" $ for_ cases test
  where
    test (factors, limit, expected) = it description assertion
      where
        description = unwords [show factors, show limit]
        assertion   = fromIntegral expression `shouldBe` expected
        expression  = sumOfMultiples (map fromIntegral factors) (fromIntegral limit)

-- Test cases adapted from `exercism/x-common/sum-of-multiples.json` on 2016-07-24.

cases :: [([Integer], Integer, Integer)]
cases = [ ( [    3,  5],     1,       0)
        , ( [    3,  5],     4,       3)
        , ( [    3,  5],    10,      23)
        , ( [    3,  5],   100,    2318)
        , ( [    3,  5],  1000,  233168)
        , ( [7, 13, 17],    20,      51)
        , ( [    4,  6],    15,      30)
        , ( [5,  6,  8],   150,    4419)
        , ( [    5, 25],    51,     275)
        , ( [   43, 47], 10000, 2203160)
        , ( [        1],   100,    4950)
        , ( [         ], 10000,       0) ]
