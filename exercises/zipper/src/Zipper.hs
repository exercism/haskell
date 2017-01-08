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
fromTree = undefined

toTree :: Zipper a -> BinTree a
toTree = undefined

value :: Zipper a -> a
value = undefined

left :: Zipper a -> Maybe (Zipper a)
left = undefined

right :: Zipper a -> Maybe (Zipper a)
right = undefined

up :: Zipper a -> Maybe (Zipper a)
up = undefined

setValue :: a -> Zipper a -> Zipper a
setValue = undefined

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft = undefined

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight = undefined
