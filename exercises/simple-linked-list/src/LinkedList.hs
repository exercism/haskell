module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

-- The task is to create the data type `LinkedList`
-- and implement the functions below.

datum :: LinkedList a -> a
datum = undefined

fromList :: [a] -> LinkedList a
fromList = undefined

isNil :: LinkedList a -> Bool
isNil = undefined

new :: a -> LinkedList a -> LinkedList a
new = undefined

next :: LinkedList a -> LinkedList a
next = undefined

nil :: LinkedList a
nil = undefined

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = undefined

toList :: LinkedList a -> [a]
toList = undefined
