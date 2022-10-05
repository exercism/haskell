{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import LeapYear (isLeapYear)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "isLeapYear" $ for_ cases test
  where

    test Case{..} = it explanation assertion
      where
        explanation = unwords [show input, "-", description]
        assertion   = isLeapYear (fromIntegral input) `shouldBe` expected

data Case = Case { description :: String
                 , input       :: Integer
                 , expected    :: Bool
                 }

cases :: [Case]
cases = [ Case { description = "year not divisible by 4 in common year"
               , input       = 2015
               , expected    = False
               }
        , Case { description = "year divisible by 2, not divisible by 4 in common year"
               , input       = 1970
               , expected    = False
               }
        , Case { description = "year divisible by 4, not divisible by 100 in leap year"
               , input       = 1996
               , expected    = True
               }
        , Case { description = "year divisible by 4 and 5 is still a leap year"
               , input       = 1960
               , expected    = True
               }
        , Case { description = "year divisible by 100, not divisible by 400 in common year"
               , input       = 2100
               , expected    = False
               }
        , Case { description = "year divisible by 100 but not by 3 is still not a leap year"
               , input       = 1900
               , expected    = False
               }
        , Case { description = "year divisible by 400 in leap year"
               , input       = 2000
               , expected    = True
               }
        , Case { description = "year divisible by 400 but not by 125 is still a leap year"
               , input       = 2400
               , expected    = True
               }
        , Case { description = "year divisible by 200, not divisible by 400 in common year"
               , input       = 1800
               , expected    = False
               }
        ]
