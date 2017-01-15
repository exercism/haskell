module Zipper
 ( BinTree(BT)
 , fromTree
 , left
 , right
 , setLeft
 , setRight
 , setValue
 , toTree
 , up
 , value
 ) where

data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

data Zipper a = Dummy deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree = error "You need to implement this function."

toTree :: Zipper a -> BinTree a
toTree = error "You need to implement this function."

value :: Zipper a -> a
value = error "You need to implement this function."

left :: Zipper a -> Maybe (Zipper a)
left = error "You need to implement this function."

right :: Zipper a -> Maybe (Zipper a)
right = error "You need to implement this function."

up :: Zipper a -> Maybe (Zipper a)
up = error "You need to implement this function."

setValue :: a -> Zipper a -> Zipper a
setValue = error "You need to implement this function."

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft = error "You need to implement this function."

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight = error "You need to implement this function."
