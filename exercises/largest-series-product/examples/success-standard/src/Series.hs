module Series
  ( Error(..)
  , largestProduct
  ) where

import Data.Char (digitToInt, isDigit)

data Error                                      
  = InvalidSpan                                 
  | InvalidDigit Char
  deriving (Show, Eq)
  
largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size > length digits || size < 0 = Left InvalidSpan
  | otherwise = do
    digits' <- traverse toDigit digits
    return . maximum $ product <$> window size digits'
  
window :: Int -> [a] -> [[a]]
window size xs
  | size <= 0 = [[]]
  | length xs < size = []
  | otherwise = take size xs : window size (tail xs)
               
toDigit :: Char -> Either Error Integer
toDigit c
  | isDigit c = Right . fromIntegral $ digitToInt c
  | otherwise = Left $ InvalidDigit c
