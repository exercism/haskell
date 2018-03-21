module Series (Error(..), largestProduct) where

import Control.Monad    ((>=>))
import Data.Char        (digitToInt, isDigit)
import Data.List        (tails)
import Data.Maybe       (mapMaybe)
import Safe             (maximumMay)
import Safe.Exact       (takeExactMay)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

rightOr :: a -> Maybe b -> Either a b
rightOr a Nothing = Left a
rightOr _ (Just x) = Right x

largestProduct :: (Integral a, Num b, Ord b) => a -> String -> Either Error b
largestProduct n = traverse charToNum >=> rightOr InvalidSpan . maximumMay . products
  where
    products = mapMaybe (fmap product . takeExactMay (fromIntegral n)) . tails
    charToNum x
        | isDigit x = Right . fromIntegral . digitToInt $ x
        | otherwise = Left (InvalidDigit x)
