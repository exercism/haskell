module DNA (count, nucleotideCounts) where

import Data.Map.Strict (Map, (!), fromDistinctAscList, fromListWith, findWithDefault)

count :: Char -> String -> Either String Int
count x xs = (!) <$> nucleotideCounts xs <*> valid x

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs = fromDistinctAscList <$> mapM count' "ACGT"
  where
    count' x = (\c -> (x, c)) <$> occur' x
    occur' x = findWithDefault 0 x . countOccurrences <$> mapM valid xs
    countOccurrences = fromListWith (+) . flip zip (repeat 1)

valid :: Char -> Either String Char
valid x
  | x `elem` "ACGT" = Right x
  | otherwise       = Left $ "invalid nucleotide " ++ show x
