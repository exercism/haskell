{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Data.String       (fromString)

import Bob (responseFor)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "responseFor" $ for_ cases test
  where
    test Case{..} = it description $ responseFor (fromString input) `shouldBe` fromString expected

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: String
                 }

cases :: [Case]
cases = [ Case { description = "stating something"
               , input       = "Tom-ay-to, tom-aaaah-to."
               , expected    = "Whatever."
               }
        , Case { description = "shouting"
               , input       = "WATCH OUT!"
               , expected    = "Whoa, chill out!"
               }
        , Case { description = "shouting gibberish"
               , input       = "FCECDFCAAB"
               , expected    = "Whoa, chill out!"
               }
        , Case { description = "asking a question"
               , input       = "Does this cryogenic chamber make me look fat?"
               , expected    = "Sure."
               }
        , Case { description = "asking a numeric question"
               , input       = "You are, what, like 15?"
               , expected    = "Sure."
               }
        , Case { description = "asking gibberish"
               , input       = "fffbbcbeab?"
               , expected    = "Sure."
               }
        , Case { description = "talking forcefully"
               , input       = "Let's go make out behind the gym!"
               , expected    = "Whatever."
               }
        , Case { description = "using acronyms in regular speech"
               , input       = "It's OK if you don't want to go to the DMV."
               , expected    = "Whatever."
               }
        , Case { description = "forceful question"
               , input       = "WHAT THE HELL WERE YOU THINKING?"
               , expected    = "Calm down, I know what I'm doing!"
               }
        , Case { description = "shouting numbers"
               , input       = "1, 2, 3 GO!"
               , expected    = "Whoa, chill out!"
               }
        , Case { description = "only numbers"
               , input       = "1, 2, 3"
               , expected    = "Whatever."
               }
        , Case { description = "question with only numbers"
               , input       = "4?"
               , expected    = "Sure."
               }
        , Case { description = "shouting with special characters"
               , input       = "ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!"
               , expected    = "Whoa, chill out!"
               }
        , Case { description = "shouting with no exclamation mark"
               , input       = "I HATE THE DMV"
               , expected    = "Whoa, chill out!"
               }
        , Case { description = "statement containing question mark"
               , input       = "Ending with ? means a question."
               , expected    = "Whatever."
               }
        , Case { description = "non-letters with question"
               , input       = ":) ?"
               , expected    = "Sure."
               }
        , Case { description = "prattling on"
               , input       = "Wait! Hang on. Are you going to be OK?"
               , expected    = "Sure."
               }
        , Case { description = "silence"
               , input       = ""
               , expected    = "Fine. Be that way!"
               }
        , Case { description = "prolonged silence"
               , input       = "          "
               , expected    = "Fine. Be that way!"
               }
        , Case { description = "alternate silence"
               , input       = "\t\t\t\t\t\t\t\t\t\t"
               , expected    = "Fine. Be that way!"
               }
        , Case { description = "multiple line question"
               , input       = "\nDoes this cryogenic chamber make me look fat?\nNo."
               , expected    = "Whatever."
               }
        , Case { description = "starting with whitespace"
               , input       = "         hmmmmmmm..."
               , expected    = "Whatever."
               }
        , Case { description = "ending with whitespace"
               , input       = "Okay if like my  spacebar  quite a bit?   "
               , expected    = "Sure."
               }
        , Case { description = "other whitespace"
               , input       = "\n\r \t"
               , expected    = "Fine. Be that way!"
               }
        , Case { description = "non-question ending with whitespace"
               , input       = "This is a statement ending with whitespace      "
               , expected    = "Whatever."
               }
        ]
