{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Data.List         (sort)
import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Change (findFewestCoins)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion  = expression `shouldBe` expected
        expression = sort <$> findFewestCoins target coins

data Case = Case { description  ::        String
                 , coins        ::       [Integer]
                 , target       ::        Integer
                 , expected     :: Maybe [Integer]
                 }

cases :: [Case]
cases = [ Case { description = "single coin change"
               , coins = [1, 5, 10, 25, 100]
               , target = 25
               , expected = Just [25]
            }
        , Case { description = "multiple coin change"
               , coins = [1, 5, 10, 25, 100]
               , target = 15
               , expected = Just [5, 10]
            }
        , Case { description = "change with Lilliputian Coins"
               , coins = [1, 4, 15, 20, 50]
               , target = 23
               , expected = Just [4, 4, 15]
            }
        , Case { description = "change with Lower Elbonia Coins"
               , coins = [1, 5, 10, 21, 25]
               , target = 63
               , expected = Just [21, 21, 21]
            }
        , Case { description = "large target values"
               , coins = [1, 2, 5, 10, 20, 50, 100]
               , target = 999
               , expected = Just [2, 2, 5, 20, 20, 50, 100, 100, 100, 100, 100, 100, 100, 100, 100]
            }
        , Case { description = "possible change without unit coins available"
               , coins = [2, 5, 10, 20, 50]
               , target = 21
               , expected = Just [2, 2, 2, 5, 10]
            }
        , Case { description = "possible change without unit coins available, must stop early on larger coin"
               , coins = [4, 5]
               , target = 27
               , expected = Just [4, 4, 4, 5, 5, 5]
             }
        , Case { description = "no coins make 0 change"
               , coins = [1, 5, 10, 21, 25]
               , target = 0
               , expected = Just []
             }
        , Case { description = "error testing for change smaller than the smallest of coins"
               , coins = [5, 10]
               , target = 3
               , expected = Nothing
             }
        , Case { description = "error if no combination can add up to target"
               , coins = [5, 10]
               , target = 94
               , expected = Nothing
             }
        , Case { description = "cannot find negative change values"
               , coins = [1, 2, 5]
               , target = -5
               , expected = Nothing
             }
        ]
