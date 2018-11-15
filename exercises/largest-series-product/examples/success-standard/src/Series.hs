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
  else do
    digits' <- traverse parseDigits digits
    return $ maximum $ product <$> window' size digits'

-- Two alternative sliding window functions
window :: Int -> [a] -> [[a]]
window size xs | size <= 0        = [[]]
               | length xs < size = []
               | otherwise        = take size xs : window size (tail xs)

window' :: Int -> [a] -> [[a]]
window' size xs = take (length xs - (size - 1)) $ map (take size) (tails xs)

parseDigits :: Char -> Either Error Integer
parseDigits c = if isDigit c
  then Right $ fromIntegral $ digitToInt c
  else Left $ InvalidDigit c
