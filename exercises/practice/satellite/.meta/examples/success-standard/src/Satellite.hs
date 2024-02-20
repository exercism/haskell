module Satellite (treeFromTraversals) where

import Data.Set (fromList, size)
import BinaryTree (BinaryTree(..))

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals preorder inorder =
  if (not.null) preorder && length preorder == size (fromList preorder)
  then treeFromTraversals' preorder inorder
  else Nothing

treeFromTraversals' :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals' preorder inorder = Branch <$> leftChild <*> pure root <*>  rightChild
  where
    root = head preorder
    (inLeft, rest) = span (/= root) inorder
    inRight = tail rest
    (preLeft, preRight) = splitAt (length inLeft) $ tail preorder
    leftChild
      | null preLeft && null inLeft = Just Leaf
      | null preLeft || null inLeft = Nothing
      | otherwise                   = treeFromTraversals' preLeft inLeft
    rightChild
      | null preRight && null inRight = Just Leaf
      | null preRight || null inRight = Nothing
      | otherwise                     = treeFromTraversals' preRight inRight
