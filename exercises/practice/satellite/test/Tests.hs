{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Satellite (treeFromTraversals)
import BinaryTree (BinaryTree(..))

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
          describe "treeFromTraversals" $ for_ cases $ test treeFromTraversals
  where
    test f Case{..} = it description $ f preorder inorder `shouldBe` expected

data Case = Case { description :: String
                 , preorder    :: String
                 , inorder     :: String
                 , expected    :: Maybe (BinaryTree Char)
                 }

cases :: [Case]
cases =
    [ Case { description = "Empty tree"
           , preorder    = ""
           , inorder     = ""
           , expected    = Nothing
           }
    , Case { description = "Tree with one item"
           , preorder    = "a"
           , inorder     = "a"
           , expected    = Just (Branch Leaf 'a' Leaf)
           }
    , Case { description = "Tree with many items"
           , preorder    = "aixfr"
           , inorder     = "iafxr"
           , expected    = Just (Branch (Branch Leaf 'i' Leaf) 'a' (Branch (Branch Leaf 'f' Leaf) 'x' (Branch Leaf 'r' Leaf)))
           }
    , Case { description = "Reject traversals of different length"
           , preorder    = "ab"
           , inorder     = "bar"
           , expected    = Nothing
           }
    , Case { description = "Reject inconsistent traversals of same length"
           , preorder    = "xyz"
           , inorder     = "abc"
           , expected    = Nothing
           }
    , Case { description = "Reject traversals with repeated items"
           , preorder    = "aba"
           , inorder     = "baa"
           , expected    = Nothing
           }
    , Case { description = "A degenerate binary tree"
           , preorder    = "abcd"
           , inorder     = "dcba"
           , expected    = Just (Branch (Branch (Branch (Branch Leaf 'd' Leaf) 'c' Leaf) 'b' Leaf) 'a' Leaf)
           }
    , Case { description = "Another degenerate binary tree"
           , preorder    = "abcd"
           , inorder     = "abcd"
           , expected    = Just (Branch Leaf 'a' (Branch Leaf 'b' (Branch Leaf 'c' (Branch Leaf 'd' Leaf))))
           }
    , Case { description = "Tree with many more items"
           , preorder    = "abdghcefi"
           , inorder     = "gdhbaecif"
           , expected    = Just (Branch (Branch (Branch (Branch Leaf 'g' Leaf) 'd' (Branch Leaf 'h' Leaf)) 'b' Leaf) 'a' (Branch (Branch Leaf 'e' Leaf) 'c' (Branch (Branch Leaf 'i' Leaf) 'f' Leaf)))
           }
    ]
