module Hamming (distance) where

distance :: String -> String -> Int
distance a b = sum . map fromEnum $ zipWith (/=) a b
