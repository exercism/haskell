module Series (largestProduct) where

import Control.Monad    ((>=>))
import Data.Char        (digitToInt, isDigit)
import Data.List        (tails)
import Data.Maybe       (mapMaybe)
import Safe             (maximumMay)
import Safe.Exact       (takeExactMay)

largestProduct :: (Integral a, Num b, Ord b) => a -> String -> Maybe b
largestProduct n = traverse charToNum >=> maximumMay . products
  where
    products = mapMaybe (fmap product . takeExactMay (fromIntegral n)) . tails
    charToNum x
        | isDigit x = Just . fromIntegral . digitToInt $ x
        | otherwise = Nothing
