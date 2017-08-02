module ProteinTranslation (toProtein) where

import Data.List.Split (chunksOf)

toProtein :: String -> [String]
toProtein strand = takeWhile ("STOP" /=) $ map codonToProtein $ chunksOf 3 strand

codonToProtein :: String -> String
codonToProtein "AUG" = "Methionine"
codonToProtein "UUU" = "Phenylalanine"
codonToProtein "UUC" = "Phenylalanine"
codonToProtein "UUA" = "Leucine"
codonToProtein "UUG" = "Leucine"
codonToProtein "UCU" = "Serine"
codonToProtein "UCC" = "Serine"
codonToProtein "UCA" = "Serine"
codonToProtein "UCG" = "Serine"
codonToProtein "UAU" = "Tyrosine"
codonToProtein "UAC" = "Tyrosine"
codonToProtein "UGU" = "Cysteine"
codonToProtein "UGC" = "Cysteine"
codonToProtein "UGG" = "Tryptophan"
codonToProtein "UAA" = "STOP"
codonToProtein "UAG" = "STOP"
codonToProtein "UGA" = "STOP"
codonToProtein _ = error "Invalid codon."
