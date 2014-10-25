-- Copy this file to `ListOps.hs` and use it as a starting point for the
-- exercise.

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
foldl' = undefined

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = undefined

length :: [a] -> Int
length = undefined

reverse :: [a] -> [a]
reverse = undefined

map :: (a -> b) -> [a] -> [b]
map = undefined

filter :: (a -> Bool) -> [a] -> [a]
filter = undefined

(++) :: [a] -> [a] -> [a]
xs ++ ys = undefined

concat :: [[a]] -> [a]
concat = undefined
