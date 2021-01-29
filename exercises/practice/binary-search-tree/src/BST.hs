module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Dummy deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft tree = error "You need to implement this function."

bstRight :: BST a -> Maybe (BST a)
bstRight tree = error "You need to implement this function."

bstValue :: BST a -> Maybe a
bstValue tree = error "You need to implement this function."

empty :: BST a
empty = error "You need to implement this function."

fromList :: Ord a => [a] -> BST a
fromList xs = error "You need to implement this function."

insert :: Ord a => a -> BST a -> BST a
insert x tree = error "You need to implement this function."

singleton :: a -> BST a
singleton x = error "You need to implement this function."

toList :: BST a -> [a]
toList tree = error "You need to implement this function."
