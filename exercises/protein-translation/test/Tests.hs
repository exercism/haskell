{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import ProteinTranslation (proteins)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "proteins" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion = proteins input `shouldBe` expected


data Case = Case { description :: String
                 , input       :: String
                 , expected    :: Maybe [String]
                 }

cases :: [Case]
cases = [ Case { description = "Methionine RNA sequence"
               , input       = "AUG"
               , expected    = Just ["Methionine"]
               }
        , Case { description = "Phenylalanine RNA sequence 1"
               , input       = "UUU"
               , expected    = Just ["Phenylalanine"]
               }
        , Case { description = "Phenylalanine RNA sequence 2"
               , input       = "UUC"
               , expected    = Just ["Phenylalanine"]
               }
        , Case { description = "Leucine RNA sequence 1"
               , input       = "UUA"
               , expected    = Just ["Leucine"]
               }
        , Case { description = "Leucine RNA sequence 2"
               , input       = "UUG"
               , expected    = Just ["Leucine"]
               }
        , Case { description = "Serine RNA sequence 1"
               , input       = "UCU"
               , expected    = Just ["Serine"]
               }
        , Case { description = "Serine RNA sequence 2"
               , input       = "UCC"
               , expected    = Just ["Serine"]
               }
        , Case { description = "Serine RNA sequence 3"
               , input       = "UCA"
               , expected    = Just ["Serine"]
               }
        , Case { description = "Serine RNA sequence 4"
               , input       = "UCG"
               , expected    = Just ["Serine"]
               }
        , Case { description = "Tyrosine RNA sequence 1"
               , input       = "UAU"
               , expected    = Just ["Tyrosine"]
               }
        , Case { description = "Tyrosine RNA sequence 2"
               , input       = "UAC"
               , expected    = Just ["Tyrosine"]
               }
        , Case { description = "Cysteine RNA sequence 1"
               , input       = "UGU"
               , expected    = Just ["Cysteine"]
               }
        , Case { description = "Cysteine RNA sequence 2"
               , input       = "UGC"
               , expected    = Just ["Cysteine"]
               }
        , Case { description = "Tryptophan RNA sequence"
               , input       = "UGG"
               , expected    = Just ["Tryptophan"]
               }
        , Case { description = "STOP codon RNA sequence 1"
               , input       = "UAA"
               , expected    = Just []
               }
        , Case { description = "STOP codon RNA sequence 2"
               , input       = "UAG"
               , expected    = Just []
               }
        , Case { description = "STOP codon RNA sequence 3"
               , input       = "UGA"
               , expected    = Just []
               }
        , Case { description = "Translate RNA strand into correct protein list"
               , input       = "AUGUUUUGG"
               , expected    = Just ["Methionine","Phenylalanine","Tryptophan"]
               }
        , Case { description = "Translation stops if STOP codon at beginning of sequence"
               , input       = "UAGUGG"
               , expected    = Just []
               }
        , Case { description = "Translation stops if STOP codon at end of two-codon sequence"
               , input       = "UGGUAG"
               , expected    = Just ["Tryptophan"]
               }
        , Case { description = "Translation stops if STOP codon at end of three-codon sequence"
               , input       = "AUGUUUUAA"
               , expected    = Just ["Methionine","Phenylalanine"]
               }
        , Case { description = "Translation stops if STOP codon in middle of three-codon sequence"
               , input       = "UGGUAGUGG"
               , expected    = Just ["Tryptophan"]
               }
        , Case { description = "Translation stops if STOP codon in middle of six-codon sequence"
               , input       = "UGGUGUUAUUAAUGGUUU"
               , expected    = Just ["Tryptophan","Cysteine","Tyrosine"]
               }
        ]
