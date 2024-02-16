module Satellite (treeFromTraversals) where

import Data.Tree (Tree)

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (Tree a)
treeFromTraversals preorder inorder = error "You need to implement this function."
