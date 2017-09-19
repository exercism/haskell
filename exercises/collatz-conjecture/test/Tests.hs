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
        assertion = collatz number `shouldBe` expected


data Case = Case { description :: String
                 , property    :: String
                 , number      :: Integer
                 , expected    :: Maybe Integer
                 }

cases :: [Case]
cases = [ Case { description = "zero steps for one"
               , property    = "steps"
               , number      = 1
               , expected    = Just 0
               }
        , Case { description = "divide if even"
               , property    = "steps"
               , number      = 16
               , expected    = Just 4
               }
        , Case { description = "even and odd steps"
               , property    = "steps"
               , number      = 12
               , expected    =Just 9
               }
        , Case { description = "Large number of even and odd steps"
               , property    = "steps"
               , number      = 1000000
               , expected    = Just 152
               }
        , Case { description = "zero is an error"
               , property    = "steps"
               , number      = 0
               , expected    = Nothing
               }
        , Case { description = "negative value is an error"
               , property    = "steps"
               , number      = -15
               , expected    = Nothing
               }
        ]
