module Satellite (treeFromTraversals) where

import Data.Tree ( Tree(Node) )
import Data.Set (fromList, size)

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (Tree a)
treeFromTraversals preorder inorder =
  if (not.null) preorder && length preorder == size (fromList preorder)
  then treeFromTraversals' preorder inorder
  else Nothing

treeFromTraversals' :: Ord a => [a] -> [a] -> Maybe (Tree a)
treeFromTraversals' preorder inorder = Node root <$> sequence (leftChild <> rightChild)
  where
    root = head preorder
    (inLeft, rest) = span (/= root) inorder
    inRight = tail rest
    (preLeft, preRight) = splitAt (length inLeft) $ tail preorder
    leftChild
      | null preLeft && null inLeft = []
      | null preLeft || null inLeft = [Nothing]
      | otherwise                   = [treeFromTraversals' preLeft inLeft]
    rightChild
      | null preRight && null inRight = []
      | null preRight || null inRight = [Nothing]
      | otherwise                   = [treeFromTraversals' preRight inRight]
