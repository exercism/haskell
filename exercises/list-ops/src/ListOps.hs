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
foldl' f z xs = error "You need to implement this function."

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z xs = error "You need to implement this function."

length :: [a] -> Int
length xs = error "You need to implement this function."

reverse :: [a] -> [a]
reverse xs = error "You need to implement this function."

map :: (a -> b) -> [a] -> [b]
map f xs = error "You need to implement this function."

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = error "You need to implement this function."

(++) :: [a] -> [a] -> [a]
xs ++ ys = error "You need to implement this function."

concat :: [[a]] -> [a]
concat xss = error "You need to implement this function."
