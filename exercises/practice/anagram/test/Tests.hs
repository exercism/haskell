{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import GHC.Exts          (fromList, toList)
import Test.Hspec        (Spec, describe, it, shouldMatchList)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Anagram (anagramsFor)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "anagramsFor" $ for_ cases test
  where

    test Case{..} = it description $ expression `shouldMatchList` expected
      where
        expression = map toList
                   . toList
                   . anagramsFor (fromList subject)
                   . fromList
                   . map fromList
                   $ candidates

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
        , Case { description = "detects two anagrams"
               , subject     = "solemn"
               , candidates  = ["lemons", "cherry", "melons"]
               , expected    = ["lemons", "melons"]
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
        , Case { description = "detects three anagrams"
               , subject     = "allergy"
               , candidates  = ["gallery", "ballerina", "regally", "clergy", "largely", "leading"]
               , expected    = ["gallery", "regally", "largely"]
               }
        , Case { description = "detects multiple anagrams with different case"
               , subject     = "nose"
               , candidates  = ["Eons", "ONES"]
               , expected    = ["Eons", "ONES"]
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
        , Case { description = "detects anagrams using case-insensitive possible matches"
               , subject     = "orchestra"
               , candidates  = ["cashregister", "Carthorse", "radishes"]
               , expected    = ["Carthorse"]
               }
        , Case { description = "does not detect an anagram if the original word is repeated"
               , subject     = "go"
               , candidates  = ["goGoGO"]
               , expected    = []
               }
        , Case { description = "anagrams must use all letters exactly once"
               , subject     = "tapper"
               , candidates  = ["patter"]
               , expected    = []
               }
        , Case { description = "words are not anagrams of themselves"
               , subject     = "BANANA"
               , candidates  = ["BANANA"]
               , expected    = []
               }
        , Case { description = "words are not anagrams of themselves even if letter case is partially different"
               , subject     = "BANANA"
               , candidates  = ["Banana"]
               , expected    = []
               }
        , Case { description = "words are not anagrams of themselves even if letter case is completely different"
               , subject     = "BANANA"
               , candidates  = ["banana"]
               , expected    = []
               }
        , Case { description = "words other than themselves can be anagrams"
               , subject     = "LISTEN"
               , candidates  = ["LISTEN", "Silent"]
               , expected    = ["Silent"]
               }
        , Case { description = "handles case of greek letters"
               , subject     = "ΑΒΓ"
               , candidates  = ["ΒΓΑ", "ΒΓΔ", "γβα", "αβγ"]
               , expected    = ["ΒΓΑ", "γβα"]
               }
        , Case { description = "different characters may have the same bytes"
               , subject     = "a⬂"
               , candidates  = ["€a"]
               , expected    = []
               }
        ]
