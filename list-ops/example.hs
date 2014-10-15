module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f = go
  where
    go acc [] = acc
    go acc (x:xs) = let acc' = f acc x in acc' `seq` go acc' xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x0 = go
  where
    go [] = x0
    go (x : xs) = x `f` go xs

length :: [a] -> Int
length = foldl' (\acc _ -> 1 + acc) 0

reverse :: [a] -> [a]
reverse = foldl' (\acc x -> x : acc) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x acc -> f x : acc) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr go []
  where go x acc | f x       = x : acc
                 | otherwise = acc

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []
