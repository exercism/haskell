{-# LANGUAGE RecordWildCards #-}

import Data.Char         (isSpace)
import Data.Foldable     (for_)
import Data.Function     (on)
import Test.Hspec        (Spec, describe, it, shouldBe, shouldMatchList)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import CryptoSquare (encode)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "encode" $ for_ cases test
  where

    test Case{..} = describe description $ do

      let shouldMatchWords  = shouldBe        `on` words
          shouldMatchString = shouldBe        `on` filter (not . isSpace)
          shouldMatchChars  = shouldMatchList `on` filter (not . isSpace)

      it "normalizes the input"    $ encode input `shouldMatchChars`  expected
      it "reorders the characters" $ encode input `shouldMatchString` expected
      it "groups the output"       $ encode input `shouldMatchWords`  expected

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: String
                 }

cases :: [Case]
cases = [ Case { description = "perfect square, all lowercase with space"
               , input       = "a dog"
               , expected    = "ao dg"
               }
        , Case { description = "perfect rectangle, mixed case"
               , input       = "A camel"
               , expected    = "am ce al"
               }
        , Case { description = "incomplete square with punctuation"
               , input       = "Wait, fox!"
               , expected    = "wtx af io"
               }
        , Case { description = "incomplete rectangle with symbols"
               , input       = "cat | cut -d@ -f1 | sort | uniq"
               , expected    = "ctoi adrq tft c1u usn"
               }
        ]
