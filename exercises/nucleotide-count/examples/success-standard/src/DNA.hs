{-# LANGUAGE TupleSections #-}
module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map.Strict (Map, fromDistinctAscList, fromListWith, findWithDefault)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = fromDistinctAscList <$> mapM count' [A,C,G,T]
  where
    count' x = (x,) <$> occur' x
    occur' x = findWithDefault 0 x . countOccurrences <$> mapM valid xs
    countOccurrences = fromListWith (+) . flip zip (repeat 1)

valid :: Char -> Either String Nucleotide
valid c = case c of
  'A' -> Right A
  'C' -> Right C
  'G' -> Right G
  'T' -> Right T
  _   -> Left $ "Invalid nucleotide " ++ show c
