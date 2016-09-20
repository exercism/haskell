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
                 , expected :: Maybe String
                 }

cases :: [Case]
cases = [ Case { number   = 1
               , expected = Just "I"
               }
        , Case { number   = 2
               , expected = Just "II"
               }
        , Case { number   = 3
               , expected = Just "III"
               }
        , Case { number   = 4
               , expected = Just "IV"
               }
        , Case { number   = 5
               , expected = Just "V"
               }
        , Case { number   = 6
               , expected = Just "VI"
               }
        , Case { number   = 9
               , expected = Just "IX"
               }
        , Case { number   = 27
               , expected = Just "XXVII"
               }
        , Case { number   = 48
               , expected = Just "XLVIII"
               }
        , Case { number   = 59
               , expected = Just "LIX"
               }
        , Case { number   = 93
               , expected = Just "XCIII"
               }
        , Case { number   = 141
               , expected = Just "CXLI"
               }
        , Case { number   = 163
               , expected = Just "CLXIII"
               }
        , Case { number   = 402
               , expected = Just "CDII"
               }
        , Case { number   = 575
               , expected = Just "DLXXV"
               }
        , Case { number   = 911
               , expected = Just "CMXI"
               }
        , Case { number   = 1024
               , expected = Just "MXXIV"
               }
        , Case { number   = 3000
               , expected = Just "MMM"
               }
        ]
