module RailFenceCipher (encode, decode) where

import Data.List (sortBy)
import Data.Function (on)

encode :: Int -> [a] -> [a]
encode n xs = concat [[a | (a, b) <- zip xs c, b == i] | i <- [1..n]]
  where c = cycle $ [1..n] ++ [n-1,n-2..2]

decode :: Int -> [a] -> [a]
decode n xs = map snd $ sortBy (compare `on` fst) zippedL
  where zippedL = zip (encode n [0..length xs - 1]) xs
