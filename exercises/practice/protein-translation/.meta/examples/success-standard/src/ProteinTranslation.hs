module ProteinTranslation(proteins) where

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
proteins [] = Just []
proteins xs
  | not (validCodon codon) = Nothing
  | protein == "STOP"      = Just []
  | otherwise              = (protein :) <$> proteins rest
  where
    (codon, rest) = splitAt 3 xs
    protein       = proteins' codon
