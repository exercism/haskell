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

bstLeft :: BST a -> Maybe (BST a)
bstLeft = undefined

bstRight :: BST a -> Maybe (BST a)
bstRight = undefined

bstValue :: BST a -> Maybe a
bstValue = undefined

empty :: BST a
empty = undefined

fromList :: Ord a => [a] -> BST a
fromList = undefined

insert :: Ord a => a -> BST a -> BST a
insert = undefined

singleton :: a -> BST a
singleton = undefined

toList :: BST a -> [a]
toList = undefined
