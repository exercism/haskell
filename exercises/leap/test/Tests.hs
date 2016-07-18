{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import LeapYear (isLeapYear)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "leap" $
          describe "isLeapYear" $ for_ cases test
  where

    test Case{..} = it explanation assertion
      where
        explanation = unwords [show input, "-", description]
        assertion   = isLeapYear (fromIntegral input) `shouldBe` expected

-- Test cases adapted from `exercism/x-common/leap.json` on 2016-07-27.

data Case = Case { description :: String
                 , input       :: Integer
                 , expected    :: Bool
                 }

cases :: [Case]
cases = [ Case { description = "leap year"
               , input       = 1996
               , expected    = True
               }
        , Case { description = "standard and odd year"
               , input       = 1997
               , expected    = False
               }
        , Case { description = "standard even year"
               , input       = 1998
               , expected    = False
               }
        , Case { description = "standard nineteenth century"
               , input       = 1900
               , expected    = False
               }
        , Case { description = "standard eighteenth century"
               , input       = 1800
               , expected    = False
               }
        , Case { description = "leap twenty fourth century"
               , input       = 2400
               , expected    = True
               }
        , Case { description = "leap y2k"
               , input       = 2000
               , expected    = True
               }
        ]
