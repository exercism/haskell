module BinarySearch (find) where

import Data.Array (Array, (!), bounds)

find :: Ord a => Array Int a -> a -> Maybe Int
find array value = find_ $ bounds array
  where
    find_ :: (Int, Int) -> Maybe Int
    find_ (i, j)
      | i > j                 = Nothing
      | value < array ! pivot = find_ (i, pivot-1)
      | value > array ! pivot = find_ (pivot+1, j)
      | otherwise             = Just pivot
      where
        pivot = (i + j) `div` 2
