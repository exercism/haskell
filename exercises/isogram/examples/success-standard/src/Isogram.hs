module Isogram (isIsogram) where

import Data.Char (isLetter, toLower)

isIsogram :: String -> Bool
isIsogram = fn . map toLower
  where fn []     = True
        fn (x:xs) = not (isLetter x && x `elem` xs) && fn xs
