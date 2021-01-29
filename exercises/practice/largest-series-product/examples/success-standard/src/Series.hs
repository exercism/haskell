module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size s
  | size > length s || size < 0 = Left InvalidSpan
  | otherwise = maximum . map product . window size <$> digits s

window :: Int -> [a] -> [[a]]
window size xs
  | size <= 0 = [[]]
  | length xs < size = []
  | otherwise = take size xs : window size (tail xs)

digits :: String -> Either Error [Integer]
digits = traverse toDigit

toDigit :: Char -> Either Error Integer
toDigit c
  | isDigit c = Right . fromIntegral . digitToInt $ c
  | otherwise = Left (InvalidDigit c)
