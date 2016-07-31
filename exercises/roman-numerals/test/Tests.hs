{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Roman (numerals)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "roman-numerals" $
          describe "numerals" $ for_ cases test
  where

    test Case{..} = it explanation assertion
      where
        explanation = show number
        assertion   = numerals (fromIntegral number) `shouldBe` expected

-- Test cases adapted from `exercism/x-common/roman-numerals.json` on 2016-07-26.

data Case = Case { number   :: Integer
                 , expected :: String
                 }

cases :: [Case]
cases = [ Case { number   = 1
               , expected = "I"
               }
        , Case { number   = 2
               , expected = "II"
               }
        , Case { number   = 3
               , expected = "III"
               }
        , Case { number   = 4
               , expected = "IV"
               }
        , Case { number   = 5
               , expected = "V"
               }
        , Case { number   = 6
               , expected = "VI"
               }
        , Case { number   = 9
               , expected = "IX"
               }
        , Case { number   = 27
               , expected = "XXVII"
               }
        , Case { number   = 48
               , expected = "XLVIII"
               }
        , Case { number   = 59
               , expected = "LIX"
               }
        , Case { number   = 93
               , expected = "XCIII"
               }
        , Case { number   = 141
               , expected = "CXLI"
               }
        , Case { number   = 163
               , expected = "CLXIII"
               }
        , Case { number   = 402
               , expected = "CDII"
               }
        , Case { number   = 575
               , expected = "DLXXV"
               }
        , Case { number   = 911
               , expected = "CMXI"
               }
        , Case { number   = 1024
               , expected = "MXXIV"
               }
        , Case { number   = 3000
               , expected = "MMM"
               }
        ]
