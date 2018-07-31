{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import IsbnVerifier (isbn)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "isbn" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion = isbn input `shouldBe` expected

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: Bool
                 }

cases :: [Case]
cases = [ Case { description = "valid isbn number"
               , input       = "3-598-21508-8"
               , expected    = True
               }
        , Case { description = "invalid isbn check digit"
               , input       = "3-598-21508-9"
               , expected    = False
               }
        , Case { description = "valid isbn number with a check digit of 10"
               , input       = "3-598-21507-X"
               , expected    = True
               }
        , Case { description = "check digit is a character other than X"
               , input       = "3-598-21507-A"
               , expected    = False
               }
        , Case { description = "invalid character in isbn"
               , input       = "3-598-P1581-X"
               , expected    = False
               }
        , Case { description = "X is only valid as a check digit"
               , input       = "3-598-2X507-9"
               , expected    = False
               }
        , Case { description = "valid isbn without separating dashes"
               , input       = "3598215088"
               , expected    = True
               }
        , Case { description = "isbn without separating dashes and X as check digit"
               , input       = "359821507"
               , expected    = False
               }
        , Case { description = "too long isbn and no dashes"
               , input       = "3598215078X"
               , expected    = False
               }
        , Case { description = "too short isbn"
               , input       = "00"
               , expected    = False
               }
        , Case { description = "isbn without check digit"
               , input       = "3-598-21507"
               , expected    = False
               }
        , Case { description = "check digit of X should not be used for 0"
               , input       = "3-598-21515-X"
               , expected    = False
               }
        , Case { description = "empty isbn"
               , input       = ""
               , expected    = False
               }
        , Case { description = "input is 9 characters"
               , input       = "134456729"
               , expected    = False
               }
        , Case { description = "invalid characters are not ignored"
               , input       = "3132P34035"
               , expected    = False
               }
        , Case { description = "input is too long but contains a valid isbn"
               , input       = "98245726788"
               , expected    = False
               }
        ]
