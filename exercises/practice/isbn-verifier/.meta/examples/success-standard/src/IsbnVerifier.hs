module IsbnVerifier (isbn) where

import Data.Char (digitToInt, isDigit)

isbn :: String -> Bool
isbn xs = length cs == 10 && all isDigit (init cs) && validCheck (last cs) && total `mod` 11 == 0
  where
    cs = filter (/= '-') xs
    validCheck c = isDigit c || c == 'X'
    value 'X' = 10
    value c   = digitToInt c
    total = sum $ zipWith (*) [10,9..1] (map value cs)
