{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Pangram (isPangram)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "isPangram" $ for_ cases test
  where
    test Case{..} = it description $ isPangram input `shouldBe` expected

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: Bool
                 }

cases :: [Case]
cases = [ Case { description = "sentence empty"
               , input       = ""
               , expected    = False
               }
        , Case { description = "recognizes a perfect lower case pangram"
               , input       = "abcdefghijklmnopqrstuvwxyz"
               , expected    = True
               }
        , Case { description = "pangram with only lower case"
               , input       = "the quick brown fox jumps over the lazy dog"
               , expected    = True
               }
        , Case { description = "missing character 'x'"
               , input       = "a quick movement of the enemy will jeopardize five gunboats"
               , expected    = False
               }
        , Case { description = "another missing character, e.g. 'h'"
               , input       = "five boxing wizards jump quickly at it"
               , expected    = False
               }
        , Case { description = "pangram with underscores"
               , input       = "the_quick_brown_fox_jumps_over_the_lazy_dog"
               , expected    = True
               }
        , Case { description = "pangram with numbers"
               , input       = "the 1 quick brown fox jumps over the 2 lazy dogs"
               , expected    = True
               }
        , Case { description = "missing letters replaced by numbers"
               , input       = "7h3 qu1ck brown fox jumps ov3r 7h3 lazy dog"
               , expected    = False
               }
        , Case { description = "pangram with mixed case and punctuation"
               , input       = "\"Five quacking Zephyrs jolt my wax bed.\""
               , expected    = True
               }
        , Case { description = "upper and lower case versions of the same character should not be counted separately"
               , input       = "the quick brown fox jumps over with lazy FX"
               , expected    = False
               }
        ]
