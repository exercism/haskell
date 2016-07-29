{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Anagram (anagramsFor)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "anagram" $
          describe "anagramsFor" $ for_ cases test
  where

    test Case{..} = it description $ expression `shouldBe` expected
      where
        expression = anagramsFor subject candidates

-- Test cases adapted from `exercism/x-common/anagrams.json` on 2016-07-25.

data Case = Case { description ::  String
                 , subject     ::  String
                 , candidates  :: [String]
                 , expected    :: [String]
                 }

cases :: [Case]
cases = [ Case { description = "no matches"
               , subject     = "diaper"
               , candidates  = [ "hello", "world", "zombies", "pants"]
               , expected    = []
               }
        , Case { description = "detects simple anagram"
               , subject     = "ant"
               , candidates  = ["tan", "stand", "at"]
               , expected    = ["tan"]
               }
        , Case { description = "does not detect false positives"
               , subject     = "galea"
               , candidates  = ["eagle"]
               , expected    = []
               }
        , Case { description = "detects multiple anagrams"
               , subject     = "master"
               , candidates  = ["stream", "pigeon", "maters"]
               , expected    = ["stream", "maters"]
               }
        , Case { description = "does not detect anagram subsets"
               , subject     = "good"
               , candidates  = ["dog", "goody"]
               , expected    = []
               }
        , Case { description = "detects anagram"
               , subject     = "listen"
               , candidates  = ["enlists", "google", "inlets", "banana"]
               , expected    = ["inlets"]
               }
        , Case { description = "detects multiple anagrams"
               , subject     = "allergy"
               , candidates  = ["gallery", "ballerina", "regally", "clergy", "largely", "leading"]
               , expected    = ["gallery", "regally", "largely"]
               }
        , Case { description = "does not detect indentical words"
               , subject     = "corn"
               , candidates  = ["corn", "dark", "Corn", "rank", "CORN", "cron", "park"]
               , expected    = ["cron"]
               }
        , Case { description = "does not detect non-anagrams with identical checksum"
               , subject     = "mass"
               , candidates  = ["last"]
               , expected    = []
               }
        , Case { description = "detects anagrams case-insensitively"
               , subject     = "Orchestra"
               , candidates  = ["cashregister", "Carthorse", "radishes"]
               , expected    = ["Carthorse"]
               }
        , Case { description = "detects anagrams using case-insensitive subject"
               , subject     = "Orchestra"
               , candidates  = ["cashregister", "carthorse", "radishes"]
               , expected    = ["carthorse"]
               }
        , Case { description = "detects anagrams using case-insensitve possible matches"
               , subject     = "orchestra"
               , candidates  = ["cashregister", "Carthorse", "radishes"]
               , expected    = ["Carthorse"]
               }
        , Case { description = "does not detect a word as its own anagram"
               , subject     = "banana"
               , candidates  = ["Banana"]
               , expected    = []
               }
        , Case { description = "does not detect a anagram if the original word is repeated"
               , subject     = "go"
               , candidates  = ["go Go GO"]
               , expected    = []
               }
        , Case { description = "anagrams must use all letters exactly once"
               , subject     = "tapper"
               , candidates  = ["patter"]
               , expected    = []
               }
        , Case { description = "eliminates anagrams with the same checksum"
               , subject     = "mass"
               , candidates  = ["last"]
               , expected    = []
               }
        , Case { description = "detects unicode anagrams"
               , subject     = "ΑΒΓ"
               , candidates  = ["ΒΓΑ", "ΒΓΔ", "γβα"]
               , expected    = ["ΒΓΑ", "γβα"]
               }
        , Case { description = "eliminates misleading unicode anagrams"
               , subject     = "ΑΒΓ"
               , candidates  = ["ABΓ"]
               , expected    = []
               }
        , Case { description = "capital word is not own anagram"
               , subject     = "BANANA"
               , candidates  = ["Banana"]
               , expected    = []
               }
        , Case { description = "anagrams must use all letters exactly once"
               , subject     = "patter"
               , candidates  = ["tapper"]
               , expected    = []
               }
        , Case { description = "accepts string arguments"
               , subject     = "ant"
               , candidates  = ["stand", "tan", "at"]
               , expected    = ["tan"]
               }
        , Case { description = "accepts single string argument"
               , subject     = "ant"
               , candidates  = ["tan"]
               , expected    = ["tan"]
               }
        ]
