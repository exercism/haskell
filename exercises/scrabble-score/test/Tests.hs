{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Scrabble (scoreLetter, scoreWord)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
          describe "scoreLetter" $ do
            it "'a'" $ scoreLetter 'a' `shouldBe`  1
            it "'Z'" $ scoreLetter 'Z' `shouldBe` 10
            it "'?'" $ scoreLetter '?' `shouldBe`  0
          describe "scoreWord" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion = scoreWord input `shouldBe` fromIntegral expected

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: Integer
                 }

cases :: [Case]
cases = [ Case { description = "lowercase letter"
               , input       = "a"
               , expected    = 1
               }
        , Case { description = "uppercase letter"
               , input       = "A"
               , expected    = 1
               }
        , Case { description = "valuable letter"
               , input       = "f"
               , expected    = 4
               }
        , Case { description = "short word"
               , input       = "at"
               , expected    = 2
               }
        , Case { description = "short, valuable word"
               , input       = "zoo"
               , expected    = 12
               }
        , Case { description = "medium word"
               , input       = "street"
               , expected    = 6
               }
        , Case { description = "medium, valuable word"
               , input       = "quirky"
               , expected    = 22
               }
        , Case { description = "long, mixed-case word"
               , input       = "OxyphenButazone"
               , expected    = 41
               }
        , Case { description = "english-like word"
               , input       = "pinata"
               , expected    = 8
               }
        , Case { description = "empty input"
               , input       = ""
               , expected    = 0
               }
        , Case { description = "entire alphabet available"
               , input       = "abcdefghijklmnopqrstuvwxyz"
               , expected    = 87
               }
        ]
