module Series (Error(..), largestProduct) where

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits = error "You need to implement this function."
