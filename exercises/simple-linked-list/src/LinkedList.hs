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

data LinkedList a = Dummy

datum :: LinkedList a -> a
datum = error "You need to implement this function."

fromList :: [a] -> LinkedList a
fromList = error "You need to implement this function."

isNil :: LinkedList a -> Bool
isNil = error "You need to implement this function."

new :: a -> LinkedList a -> LinkedList a
new = error "You need to implement this function."

next :: LinkedList a -> LinkedList a
next = error "You need to implement this function."

nil :: LinkedList a
nil = error "You need to implement this function."

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = error "You need to implement this function."

toList :: LinkedList a -> [a]
toList = error "You need to implement this function."
