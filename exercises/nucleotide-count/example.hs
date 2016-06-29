module DNA (count, nucleotideCounts) where

import qualified Control.Applicative as A
import Data.Map.Strict (Map, (!), fromDistinctAscList, fromListWith, findWithDefault)

count :: Char -> String -> Either String Int
count x xs = (!) A.<$> nucleotideCounts xs A.<*> valid x

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs = fromDistinctAscList A.<$> mapM count' "ACGT"
  where
    count' x = (\c -> (x, c)) A.<$> occur' x
    occur' x = findWithDefault 0 x . countOccurrences A.<$> mapM valid xs 
    countOccurrences = fromListWith (+) . flip zip (repeat 1)

valid :: Char -> Either String Char
valid x
  | x `elem` "ACGT" = Right x
  | otherwise       = Left $ "invalid nucleotide " ++ show x

