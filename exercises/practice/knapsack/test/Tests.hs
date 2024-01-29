{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Knapsack (maximumValue)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "valid" $ for_ cases test
  where
    test Case{..} = it description $ maximumValue input1 input2 `shouldBe` expected

data Case = Case { description :: String
                 , input1      :: Int
                 , input2      :: [(Int, Int)]
                 , expected    :: Int
                 }

cases :: [Case]
cases = [ Case { description = "no items"
               , input1      = 100
               , input2      = []
               , expected    = 0
               }
        , Case { description = "one item, too heavy"
               , input1      = 10
               , input2      = [(100, 1)]
               , expected    = 0
               }
        , Case { description = "five items (cannot be greedy by weight)"
               , input1      = 10
               , input2      = [(2, 5), (2, 5), (2, 5), (2, 5), (10, 21)]
               , expected    = 21
               }
        , Case { description = "five items (cannot be greedy by value)"
               , input1      = 10
               , input2      = [(2, 20), (2, 20), (2, 20), (2, 20), (10, 50)]
               , expected    = 80
               }
        , Case { description = "example knapsack"
               , input1      = 10
               , input2      = [(5, 10), (4, 40), (6, 30), (4, 50)]
               , expected    = 90
               }
        , Case { description = "8 items"
               , input1      = 104
               , input2      = [
                                   (25, 350),
                                   (35, 400),
                                   (45, 450),
                                   (5, 20),
                                   (25, 70),
                                   (3, 8),
                                   (2, 5),
                                   (2, 5)
                                ]
               , expected    = 900
               }
        , Case { description = "15 items"
               , input1      = 750
               , input2      = [
                                   (70, 135),
                                   (73, 139),
                                   (77, 149),
                                   (80, 150),
                                   (82, 156),
                                   (87, 163),
                                   (90, 173),
                                   (94, 184),
                                   (98, 192),
                                   (106, 201),
                                   (110, 210),
                                   (113, 214),
                                   (115, 221),
                                   (118, 229),
                                   (120, 240)
                               ]
               , expected    = 1458
               }
        ]
