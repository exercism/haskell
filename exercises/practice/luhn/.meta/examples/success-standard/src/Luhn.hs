module Luhn (isValid) where

import Data.Char (digitToInt)

luhnDouble :: Integral a => a -> a
luhnDouble n | n < 5     = n * 2
             | otherwise = n * 2 - 9

luhnDigits :: Integral a => [a] -> [a]
luhnDigits = zipWith ($) (cycle [id, luhnDouble]) . reverse

checksum :: Integral a => [a] -> a
checksum = (`rem` 10) . sum . luhnDigits

isValid :: String -> Bool
isValid s = length digits > 1 && checksum digits == 0
  where digits = map digitToInt $ filter (/= ' ') s
