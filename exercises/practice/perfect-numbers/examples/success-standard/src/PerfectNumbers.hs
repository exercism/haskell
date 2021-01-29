module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n | n < 1        = Nothing
           | n < divisors = Just Abundant
           | n > divisors = Just Deficient
           | otherwise    = Just Perfect
  where divisors = sum $ filter ((0==) . mod n) [1..(div n 2)]
