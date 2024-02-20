module BinaryTree (BinaryTree(..)) where

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)
