module CustomSet
  ( CustomSet
  , empty
  , null
  , singleton
  , union
  , toList
  , fromList
  , member
  , delete
  , difference
  , isSubsetOf
  , isDisjointFrom
  , size
  , intersection
  , insert
  ) where

-- This example is a naive unbalanced binary search tree implementation.
-- No attempt at implementing efficient hedge algorithms was made here.
-- An even more naive implementation would be to use a list.
import Prelude hiding (null)
import Data.Monoid (mempty, mappend)
import qualified Data.Foldable as F

data CustomSet a
  = Tip
  | Bin {-# UNPACK #-} !Int !a !(CustomSet a) !(CustomSet a)

instance (Ord a, Eq a) => Eq (CustomSet a) where
  a == b = size a == size b && a `isSubsetOf` b

instance Show a => Show (CustomSet a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . shows (toList xs)

instance F.Foldable CustomSet where
  foldMap f t = case t of
    Tip          -> mempty
    Bin _s a l r -> F.foldMap f l `mappend` f a `mappend` F.foldMap f r

null :: CustomSet a -> Bool
null Tip = True
null _   = False

size :: CustomSet a -> Int
size Tip              = 0
size (Bin s _a _l _r) = s

singleton :: Ord a => a -> CustomSet a
singleton x = Bin 1 x Tip Tip

toList :: CustomSet a -> [a]
toList = F.toList

empty :: Ord a => CustomSet a
empty = Tip

fromList :: Ord a => [a] -> CustomSet a
fromList = F.foldl' (flip insert) empty

insert :: Ord a => a -> CustomSet a -> CustomSet a
insert x Tip = singleton x
insert x t@(Bin s a l r) = case x `compare` a of
  LT -> let l' = insert x l in Bin (s + size l' - size l) a l' r
  GT -> let r' = insert x r in Bin (s + size r' - size r) a l r'
  EQ -> t

union :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
union = F.foldl' (flip insert)

member :: Ord a => a -> CustomSet a -> Bool
member _ Tip = False
member x (Bin _s a l r) = case x `compare` a of
  LT -> member x l
  GT -> member x r
  EQ -> True

delete :: Ord a => a -> CustomSet a -> CustomSet a
delete _ Tip             = Tip
delete x (Bin s a l r)   = case x `compare` a of
  LT -> let l' = delete x l in Bin (s + size l' - size l) a l' r
  GT -> let r' = delete x r in Bin (s + size r' - size r) a l r'
  EQ -> l `union` r

difference :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
difference = F.foldl' (flip delete)

isSubsetOf :: Ord a => CustomSet a -> CustomSet a -> Bool
isSubsetOf a b = size a <= size b && F.all (`member` b) a

intersection :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
intersection a b = F.foldl' go empty b
  where go acc x = if x `member` a then insert x acc else acc

isDisjointFrom :: Ord a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom a b = null (intersection a b)
