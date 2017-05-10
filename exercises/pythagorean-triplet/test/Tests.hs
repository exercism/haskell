{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Triplet (isPythagorean, mkTriplet, pythagoreanTriplets)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
          describe "isPythagorean"       $ for_ isPythagoreanCases       isPythagoreanTest
          describe "pythagoreanTriplets" $ for_ pythagoreanTripletsCases pythagoreanTripletsTest
  where

    isPythagoreanTest ((a, b, c), expected) = it description assertion
      where
        description = unwords $ show <$> [a, b, c]
        assertion   = isPythagorean (mkTriplet a b c) `shouldBe` expected

    pythagoreanTripletsTest (x, y, ts) = it description assertion
      where
        description = unwords $ show <$> [x, y]
        assertion   = pythagoreanTriplets x y `shouldBe` uncurry3 mkTriplet <$> ts

    uncurry3 f (x, y, z) = f x y z

    isPythagoreanCases = [ ( (3, 4, 5), True )
                         , ( (3, 5, 4), True )
                         , ( (4, 3, 5), True )
                         , ( (4, 5, 3), True )
                         , ( (5, 3, 4), True )
                         , ( (5, 4, 3), True )
                         , ( (3, 3, 3), False)
                         , ( (5, 6, 7), False) ]

    pythagoreanTripletsCases = [ (1 , 10, [ ( 3,  4,  5), ( 6,  8, 10) ])
                               , (11, 20, [ (12, 16, 20)               ])
                               , (56, 95, [ (57, 76, 95), (60, 63, 87) ]) ]
