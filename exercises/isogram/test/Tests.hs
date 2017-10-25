{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Isogram (isIsogram)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "isIsogram" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion = isIsogram input `shouldBe` expected


data Case = Case { description :: String
                 , input       :: String
                 , expected    :: Bool
                 }

cases :: [Case]
cases = [ Case { description = "empty string"
               , input       = ""
               , expected    = True
               }
        , Case { description = "isogram with only lower case characters"
               , input       = "isogram"
               , expected    = True
               }
        , Case { description = "word with one duplicated character"
               , input       = "eleven"
               , expected    = False
               }
        , Case { description = "longest reported english isogram"
               , input       = "subdermatoglyphic"
               , expected    = True
               }
        , Case { description = "word with duplicated character in mixed case"
               , input       = "Alphabet"
               , expected    = False
               }
        , Case { description = "hypothetical isogrammic word with hyphen"
               , input       = "thumbscrew-japingly"
               , expected    = True
               }
        , Case { description = "isogram with duplicated hyphen"
               , input       = "six-year-old"
               , expected    = True
               }
        , Case { description = "made-up name that is an isogram"
               , input       = "Emily Jung Schwartzkopf"
               , expected    = True
               }
        , Case { description = "duplicated character in the middle"
               , input       = "accentor"
               , expected    = False
               }
        ]
