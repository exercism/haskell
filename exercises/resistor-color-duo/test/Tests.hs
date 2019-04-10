{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import ResistorColors (Color(..), value)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "value" $ for_ cases test
  where

    test Case{..} = it explanation assertion
      where
        explanation = unwords [show input, "-", description]
        assertion   = value input `shouldBe` expected

data Case = Case { description :: String
                 , input       :: [Color]
                 , expected    :: Int
                 }

cases :: [Case]
cases = [ Case { description = "Brown and black"
               , input       = [Brown, Black]
               , expected    = 10
               }
        , Case { description = "Blue and grey"
               , input       = [Blue, Grey]
               , expected    = 68
               }
        , Case { description = "Yellow and violet"
               , input       = [Yellow, Violet]
               , expected    = 47
               }
        , Case { description = "Orange and orange"
               , input       = [Orange, Orange]
               , expected    = 33
               }
        ]
