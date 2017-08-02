{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import ProteinTranslation (toProtein)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "toProtein" $ for_ cases test
  where
    test Case{..} = it description $ toProtein strand `shouldBe` expected

data Case = Case { description :: String
                 , strand      :: String
                 , expected    :: [String]
                 }

cases :: [Case]
cases = [ Case { description = "identifies methionine codon"
               , strand      = "AUG"
               , expected    = ["Methionine"]
               }
        , Case { description = "identifies phenylalanine codon (UUU)"
               , strand      = "UUU"
               , expected    = ["Phenylalanine"]
               }
        , Case { description = "identifies phenylalanine codon (UUC)"
               , strand      = "UUC"
               , expected    = ["Phenylalanine"]
               }
        , Case { description = "identifies leucine codon (UUA)"
               , strand      = "UUA"
               , expected    = ["Leucine"]
               }
        , Case { description = "identifies leucine codon (UUG)"
               , strand      = "UUG"
               , expected    = ["Leucine"]
               }
        , Case { description = "identifies leucine codon (UUG)"
               , strand      = "UUG"
               , expected    = ["Leucine"]
               }
        , Case { description = "identifies serine codon (UCU)"
               , strand      = "UCU"
               , expected    = ["Serine"]
               }
        , Case { description = "identifies serine codon (UCC)"
               , strand      = "UCC"
               , expected    = ["Serine"]
               }
        , Case { description = "identifies serine codon (UCA)"
               , strand      = "UCA"
               , expected    = ["Serine"]
               }
        , Case { description = "identifies serine codon (UCG)"
               , strand      = "UCG"
               , expected    = ["Serine"]
               }
        , Case { description = "identifies tyrosine codon (UAU)"
               , strand      = "UAU"
               , expected    = ["Tyrosine"]
               }
        , Case { description = "identifies tyrosine codon (UAC)"
               , strand      = "UAC"
               , expected    = ["Tyrosine"]
               }
        , Case { description = "identifies cysteine codon (UGU)"
               , strand      = "UGU"
               , expected    = ["Cysteine"]
               }
        , Case { description = "identifies cysteine codon (UGC)"
               , strand      = "UGC"
               , expected    = ["Cysteine"]
               }
        , Case { description = "identifies tryptophan codon"
               , strand      = "UGG"
               , expected    = ["Tryptophan"]
               }
        , Case { description = "translate RNA strand into correct protein"
               , strand      = "AUGUUUUGG"
               , expected    = ["Methionine", "Phenylalanine", "Tryptophan"]
               }
        , Case { description = "stops translation if the STOP codon is present"
               , strand      = "AUGUUUUAA"
               , expected    = ["Methionine", "Phenylalanine"]
               }
        , Case { description = "stops translation of longest strand"
               , strand      = "UGGUGUUAUUAAUGGUUU"
               , expected    = ["Tryptophan", "Cysteine", "Tyrosine"]
               }
        ]
