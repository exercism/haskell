{-# LANGUAGE CPP #-}
module BST ( BST, bstLeft, bstRight, bstValue,
             empty, singleton, insert, fromList, toList
           ) where

#if __GLASGOW_HASKELL__ >= 710
import Data.Foldable (foldl', toList)
#else
-- Foldable, foldMap, mappend, mempty only added to Prelude in 7.10
import Data.Foldable (Foldable, foldMap, foldl', toList)
import Data.Monoid (mappend, mempty)
#endif

data BST a = Tip
           | Node (BST a) a (BST a)
           deriving (Show, Eq)

-- This allows us to use the toList from Foldable.
-- This may be seen as gratuitous for just one toList function,
-- but keep in mind now this BST could use other Foldable functions too,
-- not just toList.
instance Foldable BST where
  foldMap _f Tip = mempty
  foldMap f (Node l v r) = foldMap f l `mappend` f v `mappend` foldMap f r

bstValue :: BST a -> Maybe a
bstValue Tip = Nothing
bstValue (Node _ v _) = Just v

bstLeft :: BST a -> Maybe (BST a)
bstLeft Tip = Nothing
bstLeft (Node l _ _) = Just l

bstRight :: BST a -> Maybe (BST a)
bstRight Tip = Nothing
bstRight (Node _ _ r) = Just r

empty :: BST a
empty = Tip

singleton :: a -> BST a
singleton x = Node empty x empty

insert :: Ord a => a -> BST a -> BST a
insert x Tip = singleton x
insert x (Node l v r) =
  if v >= x
  then Node (insert x l) v r
  else Node l v (insert x r)

fromList :: Ord a => [a] -> BST a
fromList = foldl' (flip insert) empty
