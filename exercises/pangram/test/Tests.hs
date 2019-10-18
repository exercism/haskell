{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable     (for_)
import Data.String       (fromString)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Pangram (isPangram)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "isPangram" $ for_ cases test
  where
    test Case{..} = it description $ isPangram (fromString input) `shouldBe` expected

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: Bool
                 }

cases :: [Case]
cases = [ Case { description = "with empty sentence"
               , input       = ""
               , expected    = False
               }
        , Case { description = "with perfect lower case"
               , input       = "abcdefghijklmnopqrstuvwxyz"
               , expected    = True
               }
        , Case { description = "with only lower case"
               , input       = "the quick brown fox jumps over the lazy dog"
               , expected    = True
               }
        , Case { description = "with missing character 'x'"
               , input       = "a quick movement of the enemy will jeopardize five gunboats"
               , expected    = False
               }
        , Case { description = "with missing character 'h'"
               , input       = "five boxing wizards jump quickly at it"
               , expected    = False
               }
        , Case { description = "with underscores"
               , input       = "the_quick_brown_fox_jumps_over_the_lazy_dog"
               , expected    = True
               }
        , Case { description = "with numbers"
               , input       = "the 1 quick brown fox jumps over the 2 lazy dogs"
               , expected    = True
               }
        , Case { description = "with missing letters replaced by numbers"
               , input       = "7h3 qu1ck brown fox jumps ov3r 7h3 lazy dog"
               , expected    = False
               }
        , Case { description = "with mixed case and punctuation"
               , input       = "\"Five quacking Zephyrs jolt my wax bed.\""
               , expected    = True
               }
        , Case { description = "with mixed case"
               , input       = "the quick brown fox jumps over with lazy FX"
               , expected    = False
               }
        , Case { description = "with missing character and non-ascii letters"
               , input       = "abcdefghijklmnopqrstuvwxyÃ"
               , expected    = False
               }
        , Case { description = "with additional non-ascii letters"
               , input       = "abcdefghijklmnopqrstuvwxyzÃ"
               , expected    = True
               }

        {-
        -- The following test can be enabled for String-based solutions:
        , Case { description = "with termination as soon as all letters have occurred"
               , input       = "abcdefghijklmnopqrstuvwxyz" ++ [undefined]
               , expected    = True
               }
        -- -}
        ]
