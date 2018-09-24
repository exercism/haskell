module Series
  ( Error(..)
  , largestProduct
  )
where

import           Data.Char
import           Data.List                      ( tails )

data Error
  = InvalidSpan
  | InvalidDigit Char
  deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits = if size > length digits || size < 0
  then Left InvalidSpan
  else maximum <$> mapM product' (window' size digits')
 where
  digits' :: [Either Error Integer]
  digits' = map parsec digits

window :: Int -> [a] -> [[a]]
window size xs | size <= 0        = [[]]
               | length xs < size = []
               | otherwise        = take size xs : window size (tail xs)

window' :: Int -> [a] -> [[a]]
window' size xs = take (length xs - (size - 1)) $ map (take size) (tails xs)

parsec :: Char -> Either Error Integer
parsec c = if isDigit c
  then Right $ fromIntegral $ digitToInt c
  else Left $ InvalidDigit c

product' :: [Either Error Integer] -> Either Error Integer
product' xs = product <$> sequence xs
-- product' = (product <$>) . sequence
~                                                 
