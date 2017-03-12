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
delete x set = error "You need to implement this function."

difference :: CustomSet a -> CustomSet a -> CustomSet a
difference setA setB = error "You need to implement this function."

empty :: CustomSet a
empty = error "You need to implement this function."

fromList :: [a] -> CustomSet a
fromList xs = error "You need to implement this function."

insert :: a -> CustomSet a -> CustomSet a
insert x set = error "You need to implement this function."

intersection :: CustomSet a -> CustomSet a -> CustomSet a
intersection setA setB = error "You need to implement this function."

isDisjointFrom :: CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = error "You need to implement this function."

isSubsetOf :: CustomSet a -> CustomSet a -> Bool
isSubsetOf setA setB = error "You need to implement this function."

member :: a -> CustomSet a -> Bool
member x set = error "You need to implement this function."

null :: CustomSet a -> Bool
null set = error "You need to implement this function."

size :: CustomSet a -> Int
size set = error "You need to implement this function."

toList :: CustomSet a -> [a]
toList set = error "You need to implement this function."

union :: CustomSet a -> CustomSet a -> CustomSet a
union setA setB = error "You need to implement this function."
