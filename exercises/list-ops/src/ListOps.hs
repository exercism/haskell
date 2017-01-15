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
foldl' = error "You need to implement this function."

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = error "You need to implement this function."

length :: [a] -> Int
length = error "You need to implement this function."

reverse :: [a] -> [a]
reverse = error "You need to implement this function."

map :: (a -> b) -> [a] -> [b]
map = error "You need to implement this function."

filter :: (a -> Bool) -> [a] -> [a]
filter = error "You need to implement this function."

(++) :: [a] -> [a] -> [a]
xs ++ ys = error "You need to implement this function."

concat :: [[a]] -> [a]
concat = error "You need to implement this function."
