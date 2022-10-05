{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable     (for_)
import Data.List         (sort)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Triplet (tripletsWithSum)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "pythagoreanTriplets" $ for_ cases test
  where
    test (description, n, ts) = it description $ sort (tripletsWithSum n) `shouldBe` ts
    cases = [ ("triplets whose sum is 12"
              , 12
              , [(3, 4, 5)]
              )
            , ("triplets whose sum is 108"
              , 108
              , [(27, 36, 45)]
              )
            , ("triplets whose sum is 1000"
              , 1000
              , [(200, 375, 425)]
              )
            , ("no matching triplets for 1001"
              , 1001
              , []
              )
            , ("returns all matching triplets"
              , 90
              , [ (9, 40, 41)
                , (15, 36, 39)
                ]
              )
            , ("several matching triplets"
              , 840
              , [ (40, 399, 401)
                , (56, 390, 394)
                , (105, 360, 375)
                , (120, 350, 370)
                , (140, 336, 364)
                , (168, 315, 357)
                , (210, 280, 350)
                , (240, 252, 348)
                ]
              )
            , ("triplets for large number"
              , 30000
              , [ (1200, 14375, 14425)
                , (1875, 14000, 14125)
                , (5000, 12000, 13000)
                , (6000, 11250, 12750)
                , (7500, 10000, 12500)
                ]
              )
            ]
