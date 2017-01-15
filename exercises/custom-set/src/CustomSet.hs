module CustomSet
  ( delete
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

data CustomSet a = Dummy deriving (Eq, Show)

delete :: a -> CustomSet a -> CustomSet a
delete = error "You need to implement this function."

difference :: CustomSet a -> CustomSet a -> CustomSet a
difference = error "You need to implement this function."

empty :: CustomSet a
empty = error "You need to implement this function."

fromList :: [a] -> CustomSet a
fromList = error "You need to implement this function."

insert :: a -> CustomSet a -> CustomSet a
insert = error "You need to implement this function."

intersection :: CustomSet a -> CustomSet a -> CustomSet a
intersection = error "You need to implement this function."

isDisjointFrom :: CustomSet a -> CustomSet a -> Bool
isDisjointFrom = error "You need to implement this function."

isSubsetOf :: CustomSet a -> CustomSet a -> Bool
isSubsetOf = error "You need to implement this function."

member :: a -> CustomSet a -> Bool
member = error "You need to implement this function."

null :: CustomSet a -> Bool
null = error "You need to implement this function."

size :: CustomSet a -> Int
size = error "You need to implement this function."

toList :: CustomSet a -> [a]
toList = error "You need to implement this function."

union :: CustomSet a -> CustomSet a -> CustomSet a
union = error "You need to implement this function."
