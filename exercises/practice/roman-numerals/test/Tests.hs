{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Roman (numerals)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

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
cases = [ Case { description = "1 is I"
               , number      = 1
               , expected    = Just "I"
               }
        , Case { description = "2 is II"
               , number      = 2
               , expected    = Just "II"
               }
        , Case { description = "3 is III"
               , number      = 3
               , expected    = Just "III"
               }
        , Case { description = "4 is IV"
               , number      = 4
               , expected    = Just "IV"
               }
        , Case { description = "5 is V"
               , number      = 5
               , expected    = Just "V"
               }
        , Case { description = "6 is VI"
               , number      = 6
               , expected    = Just "VI"
               }
        , Case { description = "9 is IX"
               , number      = 9
               , expected    = Just "IX"
               }
        , Case { description = "16 is XVI"
               , number      = 16
               , expected    = Just "XVI"
               }
        , Case { description = "27 is XXVII"
               , number      = 27
               , expected    = Just "XXVII"
               }
        , Case { description = "48 is XLVIII"
               , number      = 48
               , expected    = Just "XLVIII"
               }
        , Case { description = "49 is XLIX"
               , number      = 49
               , expected    = Just "XLIX"
               }
        , Case { description = "59 is LIX"
               , number      = 59
               , expected    = Just "LIX"
               }
        , Case { description = "66 is LXVI"
               , number      = 66
               , expected    = Just "LXVI"
               }
        , Case { description = "93 is XCIII"
               , number      = 93
               , expected    = Just "XCIII"
               }
        , Case { description = "141 is CXLI"
               , number      = 141
               , expected    = Just "CXLI"
               }
        , Case { description = "163 is CLXIII"
               , number      = 163
               , expected    = Just "CLXIII"
               }
        , Case { description = "166 is CLXVI"
               , number      = 166
               , expected    = Just "CLXVI"
               }
        , Case { description = "402 is CDII"
               , number      = 402
               , expected    = Just "CDII"
               }
        , Case { description = "575 is DLXXV"
               , number      = 575
               , expected    = Just "DLXXV"
               }
        , Case { description = "666 is DCLXVI"
               , number      = 666
               , expected    = Just "DCLXVI"
               }
        , Case { description = "911 is CMXI"
               , number      = 911
               , expected    = Just "CMXI"
               }
        , Case { description = "1024 is MXXIV"
               , number      = 1024
               , expected    = Just "MXXIV"
               }
        , Case { description = "1666 is MDCLXVI"
               , number      = 1666
               , expected    = Just "MDCLXVI"
               }
        , Case { description = "3000 is MMM"
               , number      = 3000
               , expected    = Just "MMM"
               }
        , Case { description = "3001 is MMMI"
               , number      = 3001
               , expected    = Just "MMMI"
               }
        , Case { description = "3888 is MMMDCCCLXXXVIII"
               , number      = 3888
               , expected    = Just "MMMDCCCLXXXVIII"
               }
        , Case { description = "3999 is MMMCMXCIX"
               , number      = 3999
               , expected    = Just "MMMCMXCIX"
               }
        , Case { description = "4000 cannot be represented"
               , number      = 4000
               , expected    = Nothing
               }
        , Case { description = "0 cannot be represented"
               , number      = 0
               , expected    = Nothing
               }
        , Case { description = "-1 cannot be represented"
               , number      = -1
               , expected    = Nothing
               }
        ]
