module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals preorder inorder = error "You need to implement this function."
