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

data LinkedList a = Dummy deriving (Eq, Show)

datum :: LinkedList a -> a
datum linkedList = error "You need to implement this function."

fromList :: [a] -> LinkedList a
fromList xs = error "You need to implement this function."

isNil :: LinkedList a -> Bool
isNil linkedList = error "You need to implement this function."

new :: a -> LinkedList a -> LinkedList a
new x linkedList = error "You need to implement this function."

next :: LinkedList a -> LinkedList a
next linkedList = error "You need to implement this function."

nil :: LinkedList a
nil = error "You need to implement this function."

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList linkedList = error "You need to implement this function."

toList :: LinkedList a -> [a]
toList linkedList = error "You need to implement this function."
