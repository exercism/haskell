{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import OCR (convert)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "convert" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion = convert (unlines input) `shouldBe` expected

data Case = Case { description ::  String
                 , expected    ::  String
                 , input       :: [String]
                 }

cases :: [Case]
cases = [ Case { description = "Recognizes 0"
               , expected    = "0"
               , input       = [ " _ "
                               , "| |"
                               , "|_|"
                               , "   " ]
               }
        , Case { description = "Recognizes 1"
               , expected    = "1"
               , input       = [ "   "
                               , "  |"
                               , "  |"
                               , "   " ]
               }
        , Case { description = "Unreadable but correctly sized inputs return ?"
               , expected    = "?"
               , input       = [ "   "
                               , "  _"
                               , "  |"
                               , "   " ]
               }

        {- In this track, the tests to determine if the input
           has the correct format where not implemented.

        , Case { description = "Input with a number of lines that is not a multiple of four raises an error"
               , expected    = -1
               , input       = [ " _ "
                               , "| |"
                               , "   " ]
               }
        , Case { description = "Input with a number of columns that is not a multiple of three raises an error"
               , expected    = -1
               , input       = [ "    "
                               , "   |"
                               , "   |"
                               , "    " ]
               }
        -}

        , Case { description = "Recognizes 110101100"
               , expected    = "110101100"
               , input       = [ "       _     _        _  _ "
                               , "  |  || |  || |  |  || || |"
                               , "  |  ||_|  ||_|  |  ||_||_|"
                               , "                           " ]
               }
        , Case { description = "Garbled numbers in a string are replaced with ?"
               , expected    = "11?10?1?0"
               , input       = [ "       _     _           _ "
                               , "  |  || |  || |     || || |"
                               , "  |  | _|  ||_|  |  ||_||_|"
                               , "                           " ]
               }
        , Case { description = "Recognizes 2"
               , expected    = "2"
               , input       = [ " _ "
                               , " _|"
                               , "|_ "
                               , "   " ]
               }
        , Case { description = "Recognizes 3"
               , expected    = "3"
               , input       = [ " _ "
                               , " _|"
                               , " _|"
                               , "   " ]
               }
        , Case { description = "Recognizes 4"
               , expected    = "4"
               , input       = [ "   "
                               , "|_|"
                               , "  |"
                               , "   " ]
               }
        , Case { description = "Recognizes 5"
               , expected    = "5"
               , input       = [ " _ "
                               , "|_ "
                               , " _|"
                               , "   " ]
               }
        , Case { description = "Recognizes 6"
               , expected    = "6"
               , input       = [ " _ "
                               , "|_ "
                               , "|_|"
                               , "   " ]
               }
        , Case { description = "Recognizes 7"
               , expected    = "7"
               , input       = [ " _ "
                               , "  |"
                               , "  |"
                               , "   " ]
               }
        , Case { description = "Recognizes 8"
               , expected    = "8"
               , input       = [ " _ "
                               , "|_|"
                               , "|_|"
                               , "   " ]
               }
        , Case { description = "Recognizes 9"
               , expected    = "9"
               , input       = [ " _ "
                               , "|_|"
                               , " _|"
                               , "   " ]
               }
        , Case { description = "Recognizes string of decimal numbers"
               , expected    = "1234567890"
               , input       = [ "    _  _     _  _  _  _  _  _ "
                               , "  | _| _||_||_ |_   ||_||_|| |"
                               , "  ||_  _|  | _||_|  ||_| _||_|"
                               , "                              " ]
               }
        , Case { description = "Numbers separated by empty lines are recognized. Lines are joined by commas."
               , expected    = "123,456,789"
               , input       = [ "    _  _ "
                               , "  | _| _|"
                               , "  ||_  _|"
                               , "         "
                               , "    _  _ "
                               , "|_||_ |_ "
                               , "  | _||_|"
                               , "         "
                               , " _  _  _ "
                               , "  ||_||_|"
                               , "  ||_| _|"
                               , "         " ]
               }
        ]
