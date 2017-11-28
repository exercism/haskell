module RailFenceCipher (encode, decode) where

import Data.List (sort)

encode :: (Ord a) => Int -> [a] -> [a]
encode n xs = concat [[a | (a, b) <- zip xs c, b == i] | i <- [1..n]]
  where c = cycle $ [1..n] ++ [n-1,n-2..2]

decode :: (Ord a) => Int -> [a] -> [a]
decode n xs = map snd $ sort $ zip (encode n [0..length xs - 1]) xs
