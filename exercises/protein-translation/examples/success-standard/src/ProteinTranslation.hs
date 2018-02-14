module ProteinTranslation(proteins) where

import Data.List.Split (chunksOf)

validCodon :: String -> Bool
validCodon x = x `elem` ["AUG","UUU","UUC","UUA","UUG","UCU","UCC","UCA","UCG",
                         "UAU","UAC","UGU","UGC","UGG","UAA","UAG","UGA"]

proteins' :: String -> String
proteins' x | x == "AUG"                         = "Methionine"
            | x == "UGG"                         = "Tryptophan"
            | x `elem` ["UUU","UUC"]             = "Phenylalanine"
            | x `elem` ["UUA","UUG"]             = "Leucine"
            | x `elem` ["UCU","UCC","UCA","UCG"] = "Serine"
            | x `elem` ["UAU","UAC"]             = "Tyrosine"
            | x `elem` ["UGU","UGC"]             = "Cysteine"
            | otherwise                          = "STOP"

proteins :: String -> Maybe [String]
proteins xs = if length xs `mod` 3 == 0 && all validCodon codons
                then Just (takeWhile (/= "STOP") (map proteins' codons))
                else Nothing
  where
    codons = chunksOf 3 xs
