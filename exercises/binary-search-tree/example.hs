module BST ( BST, bstLeft, bstRight, bstValue,
             singleton, insert, fromList, toList
           ) where
import Data.List (foldl')

data BST a = Tip
           | Node (BST a) a (BST a)

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

singleton :: Ord a => a -> BST a
singleton x = Node empty x empty

insert :: Ord a => a -> BST a -> BST a
insert x Tip = singleton x
insert x (Node l v r) =
  if v >= x
  then Node (insert x l) v r
  else Node l v (insert x r)

fromList :: Ord a => [a] -> BST a
fromList = foldl' (flip insert) empty

toList :: BST a -> [a]
toList Tip = []
toList (Node l v r) = toList l ++ [v] ++ toList r
