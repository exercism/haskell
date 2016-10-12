module Anagram (anagramsFor) where
import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor word = filter (isAnagram . normalize)
  where
    normalize xs = let nxs = map toLower xs in (nxs, sort nxs)
    (nw, sw) = normalize word
    isAnagram (w, s) = nw /= w && sw == s
