module Isogram (isIsogram) where

import Data.Char (isLetter, toLower)

isIsogram :: String -> Bool
isIsogram = fn . map toLower
  where fn []     = True
        fn (x:xs) = if isLetter x && x `elem` xs then False else fn xs
