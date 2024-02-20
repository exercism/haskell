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
    ]
