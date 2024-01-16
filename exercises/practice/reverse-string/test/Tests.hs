{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import ReverseString (reverseString)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "reverseString" $ for_ cases test
  where

    test Case{..} = it explanation assertion
      where
        explanation = unwords [show input, "-", description]
        assertion   = reverseString input `shouldBe` expected

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: String
                 }

cases :: [Case]
cases = [ Case { description = "empty string"
               , input       = ""
               , expected    = ""
               }
        , Case { description = "a word"
               , input       = "robot"
               , expected    = "tobor"
               }
        , Case { description = "a capitalized word"
               , input       = "Ramen"
               , expected    = "nemaR"
               }
        , Case { description = "a sentence with punctuation"
               , input       = "I am hungry!"
               , expected    = "!yrgnuh ma I"
               }
        , Case { description = "a palindrome"
               , input       = "racecar"
               , expected    = "racecar"
               }
        , Case { description = "an even-sized word"
               , input       = "drawer"
               , expected    = "reward"
               }
        ]
