{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import PerfectNumbers (Classification(Deficient, Perfect, Abundant), classify)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "classify" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion = classify number `shouldBe` expected


data Case = Case { description :: String
                 , number      :: Int
                 , expected    :: Maybe Classification
                 }

cases :: [Case]
cases = [ Case { description = "Smallest perfect number is classified correctly"
               , number      = 6
               , expected    = Just Perfect
               }
        , Case { description = "Medium perfect number is classified correctly"
               , number      = 28
               , expected    = Just Perfect
               }
        , Case { description = "Large perfect number is classified correctly"
               , number      = 33550336
               , expected    = Just Perfect
               }
        , Case { description = "Smallest abundant number is classified correctly"
               , number      = 12
               , expected    = Just Abundant
               }
        , Case { description = "Medium abundant number is classified correctly"
               , number      = 30
               , expected    = Just Abundant
               }
        , Case { description = "Large abundant number is classified correctly"
               , number      = 33550335
               , expected    = Just Abundant
               }
        , Case { description = "Smallest prime deficient number is classified correctly"
               , number      = 2
               , expected    = Just Deficient
               }
        , Case { description = "Smallest non-prime deficient number is classified correctly"
               , number      = 4
               , expected    = Just Deficient
               }
        , Case { description = "Medium deficient number is classified correctly"
               , number      = 32
               , expected    = Just Deficient
               }
        , Case { description = "Large deficient number is classified correctly"
               , number      = 33550337
               , expected    = Just Deficient
               }
        , Case { description = "Edge case (no factors other than itself) is classified correctly"
               , number      = 1
               , expected    = Just Deficient
               }
        , Case { description = "Zero is rejected (not a natural number)"
               , number      = 0
               , expected    = Nothing
               }
        , Case { description = "Negative integer is rejected (not a natural number)"
               , number      = -1
               , expected    = Nothing
               }
        ]
