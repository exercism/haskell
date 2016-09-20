module CustomSet
  ( CustomSet
  , delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null)

delete :: a -> CustomSet a -> CustomSet a
delete = undefined

difference :: CustomSet a -> CustomSet a -> CustomSet a
difference = undefined

empty :: CustomSet a
empty = undefined

fromList :: [a] -> CustomSet a
fromList = undefined

insert :: a -> CustomSet a -> CustomSet a
insert = undefined

intersection :: CustomSet a -> CustomSet a -> CustomSet a
intersection = undefined

isDisjointFrom :: CustomSet a -> CustomSet a -> Bool
isDisjointFrom = undefined

isSubsetOf :: CustomSet a -> CustomSet a -> Bool
isSubsetOf = undefined

member :: a -> CustomSet a -> Bool
member = undefined

null :: CustomSet a -> Bool
null = undefined

size :: CustomSet a -> Int
size = undefined

toList :: CustomSet a -> [a]
toList = undefined

union :: CustomSet a -> CustomSet a -> CustomSet a
union = undefined
