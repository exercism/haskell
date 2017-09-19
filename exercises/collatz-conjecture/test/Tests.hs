{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import CollatzConjecture (collatz)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "collatz" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion = collatz number `shouldBe` fromIntegral expected


data Case = Case { description :: String
                 , number      :: Integer
                 , expected    :: Integer
                 }

cases :: [Case]
cases = [ Case { description = "collatz 1 don't require any steps."
               , binary      = 1
               , expected    = 0
               }
        , Case { description = "collatz 2 require 1 step."
               , binary      = 2
               , expected    = 1
               }
        , Case { description = "collatz 3 require 7 steps."
               , binary      = 3
               , expected    = 7
               }
        , Case { description = "collatz 4 require 2 steps."
               , binary      = 4
               , expected    = 2
               }
        , Case { description = "collatz 5 require 5 steps."
               , binary      = 5
               , expected    = 5
               }
        , Case { description = "collatz 6 require 8 steps."
               , binary      = 6
               , expected    = 8
               }
        , Case { description = "collatz 7 require 16 steps."
               , binary      = 7
               , expected    = 16
               }
        , Case { description = "collatz 666 require 113 steps."
               , binary      = 666
               , expected    = 113
               }
        ]
