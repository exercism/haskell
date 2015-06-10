{-# LANGUAGE BangPatterns #-}
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
foldl' f !b (a:as)  = foldl' f (f b a) as
foldl' _ b _       = b

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b (a:as) = f a (foldr f b as)
foldr _ b _      = b

length :: [a] -> Int
length = foldr (\_ -> (+) 1) 0

--Non-exhaustive patterns in reverse, map, and filter. Not sure but I think all cases are actually covered by using foldr since the base case of [] is the accumulator and there's no case I can think of for _ _ that wouldn't have been reached by the rest. Maybe I'm wrong.
reverse :: [a] -> [a]
reverse = foldr (func) []
  where
    func a [acc] = acc : [a]
    func a []    = a : []

map :: (a -> b) -> [a] -> [b]
map f a = foldr (\x [acc] -> f x : [acc]) [] a

filter :: (a -> Bool) -> [a] -> [a]
filter f a = foldr (\x [acc] -> if f x then x : [acc] else [acc]) [] a

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (\x y -> x : y) ys xs

concat :: [[a]] -> [a]
concat (x:xs) = x ++ concat xs
concat []     = []