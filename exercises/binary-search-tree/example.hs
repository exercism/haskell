module BST ( BST, bstLeft, bstRight, bstValue,
             singleton, insert, fromList, toList
           ) where
import Data.List (foldl')

data Node a = Node { nodeValue :: a
                   , nodeLeft :: BST a
                   , nodeRight :: BST a }
              deriving (Show, Eq)
-- We use newtype instead of type here,
-- because e.g. Foldable for Maybe doesn't make sense for BST.
newtype BST a = BST (Maybe (Node a)) deriving (Show, Eq)

fromBST :: BST a -> Maybe (Node a)
fromBST (BST t) = t

bstValue :: BST a -> Maybe a
bstValue = fmap nodeValue . fromBST

bstLeft :: BST a -> Maybe (BST a)
bstLeft = fmap nodeLeft . fromBST

bstRight :: BST a -> Maybe (BST a)
bstRight = fmap nodeRight . fromBST

empty :: BST a
empty = BST Nothing

singleton :: Ord a => a -> BST a
singleton x = BST (Just (Node x empty empty))

insert :: Ord a => a -> BST a -> BST a
insert x = maybe (singleton x) (BST . Just . insert' x) . fromBST

insert' :: Ord a => a -> Node a -> Node a
insert' x n =
  if nodeValue n >= x
  then n { nodeLeft  = insert x (nodeLeft  n) }
  else n { nodeRight = insert x (nodeRight n) }

fromList :: Ord a => [a] -> BST a
fromList = foldl' (flip insert) empty

toList :: BST a -> [a]
toList (BST Nothing) = []
toList (BST (Just (Node x l r))) = toList l ++ [x] ++ toList r
