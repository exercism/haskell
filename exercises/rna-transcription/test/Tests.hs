{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import DNA (toRNA)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "toRNA" $ for_ cases test
  where
    test Case{..} = it description $ toRNA dna `shouldBe` expected

data Case = Case { description ::       String
                 , dna         ::       String
                 , expected    :: Maybe String
                 }

cases :: [Case]
cases = [ Case { description = "rna complement of cytosine is guanine"
               , dna         =      "C"
               , expected    = Just "G"
               }
        , Case { description = "rna complement of guanine is cytosine"
               , dna         =      "G"
               , expected    = Just "C"
               }
        , Case { description = "rna complement of thymine is adenine"
               , dna         =      "T"
               , expected    = Just "A"
               }
        , Case { description = "rna complement of adenine is uracil"
               , dna         =      "A"
               , expected    = Just "U"
               }
        , Case { description = "rna complement"
               , dna         =      "ACGTGGTCTTAA"
               , expected    = Just "UGCACCAGAAUU"
               }
        , Case { description = "dna correctly handles invalid input"
               , dna         = "U"
               , expected    = Nothing
               }
        , Case { description = "dna correctly handles completely invalid input"
               , dna         = "XXX"
               , expected    = Nothing
               }
        , Case { description = "dna correctly handles partially invalid input"
               , dna         = "ACGTXXXCTTAA"
               , expected    = Nothing
               }
        ]
