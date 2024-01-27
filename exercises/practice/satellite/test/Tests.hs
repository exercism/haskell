{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)
import Data.Tree         (Tree(Node))

import Satellite (treeFromTraversals)

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
                 , expected    :: Maybe (Tree Char)
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
           , expected    = Just (Node 'a' [])
           }
    , Case { description = "Tree with many items"
           , preorder    = "aixfr"
           , inorder     = "iafxr"
           , expected    = Just (Node 'a' [Node 'i' [], Node 'x' [Node 'f' [], Node 'r' []]])
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
