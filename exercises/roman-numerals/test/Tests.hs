{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Roman (numerals)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "numerals" $ for_ cases test
  where

    test Case{..} = it explanation assertion
      where
        explanation = show number ++ ": " ++ description
        assertion   = numerals (fromIntegral number) `shouldBe` expected

data Case = Case { description :: String
                 , number      :: Integer
                 , expected    :: Maybe String
                 }

cases :: [Case]
cases = [ Case { description = "1 is a single I"
               , number      = 1
               , expected    = Just "I"
               }
        , Case { description = "2 is two I's"
               , number      = 2
               , expected    = Just "II"
               }
        , Case { description = "3 is three I's"
               , number      = 3
               , expected    = Just "III"
               }
        , Case { description = "4, being 5 - 1, is IV"
               , number      = 4
               , expected    = Just "IV"
               }
        , Case { description = "5 is a single V"
               , number      = 5
               , expected    = Just "V"
               }
        , Case { description = "6, being 5 + 1, is VI"
               , number      = 6
               , expected    = Just "VI"
               }
        , Case { description = "9, being 10 - 1, is IX"
               , number      = 9
               , expected    = Just "IX"
               }
        , Case { description = "20 is two X's"
               , number      = 27
               , expected    = Just "XXVII"
               }
        , Case { description = "48 is not 50 - 2 but rather 40 + 8"
               , number      = 48
               , expected    = Just "XLVIII"
               }
        , Case { description = "49 is not 40 + 5 + 4 but rather 50 - 10 + 10 - 1"
               , number      = 49
               , expected    = Just "XLIX"
               }
        , Case { description = "50 is a single L"
               , number      = 59
               , expected    = Just "LIX"
               }
        , Case { description = "90, being 100 - 10, is XC"
               , number      = 93
               , expected    = Just "XCIII"
               }
        , Case { description = "100 is a single C"
               , number      = 141
               , expected    = Just "CXLI"
               }
        , Case { description = "60, being 50 + 10, is LX"
               , number      = 163
               , expected    = Just "CLXIII"
               }
        , Case { description = "400, being 500 - 100, is CD"
               , number      = 402
               , expected    = Just "CDII"
               }
        , Case { description = "500 is a single D"
               , number      = 575
               , expected    = Just "DLXXV"
               }
        , Case { description = "900, being 1000 - 100, is CM"
               , number      = 911
               , expected    = Just "CMXI"
               }
        , Case { description = "1000 is a single M"
               , number      = 1024
               , expected    = Just "MXXIV"
               }
        , Case { description = "3000 is three M's"
               , number      = 3000
               , expected    = Just "MMM"
               }
        ]
