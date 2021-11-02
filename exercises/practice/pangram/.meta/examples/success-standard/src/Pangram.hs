module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram xs = all (`elem` fixedText) ['a'..'z']
  where fixedText = map toLower xs
