module Isogram (isIsogram) where

import Data.Char (toLower, isLetter)
import Data.List (sort, group)

isIsogram :: String -> Bool
isIsogram = all ((1==) . length) . group . sort . filter isLetter . map toLower
