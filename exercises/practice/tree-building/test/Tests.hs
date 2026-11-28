{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import TreeBuilding      (Record(..), Tree, buildTree, node)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "buildTree" $ for_ cases test
  where
    test Case{..} = it description $ buildTree input `shouldBe` expected

data Case = Case { description :: String
                 , input       :: [Record]
                 , expected    :: Maybe Tree
                 }

cases :: [Case]
cases =
  [ Case { description = "empty list"
         , input       = []
         , expected    = Nothing
         }
  , Case { description = "single record"
         , input       = [Record 0 0]
         , expected    = Just (node 0 [])
         }
  , Case { description = "three records in order"
         , input       = [Record 0 0, Record 1 0, Record 2 0]
         , expected    = Just (node 0 [node 1 [], node 2 []])
         }
  , Case { description = "three records in reverse order"
         , input       = [Record 2 0, Record 1 0, Record 0 0]
         , expected    = Just (node 0 [node 1 [], node 2 []])
         }
  , Case { description = "more than two children"
         , input       = [Record 0 0, Record 1 0, Record 2 0, Record 3 0]
         , expected    = Just (node 0 [node 1 [], node 2 [], node 3 []])
         }
  , Case { description = "binary tree"
         , input       = [Record 5 1, Record 3 2, Record 2 0, Record 4 1, Record 1 0, Record 0 0, Record 6 2]
         , expected    = Just (node 0 [node 1 [node 4 [], node 5 []], node 2 [node 3 [], node 6 []]])
         }
  , Case { description = "unbalanced tree"
         , input       = [Record 5 2, Record 3 2, Record 2 0, Record 4 1, Record 1 0, Record 0 0, Record 6 2]
         , expected    = Just (node 0 [node 1 [node 4 []], node 2 [node 3 [], node 5 [], node 6 []]])
         }
  , Case { description = "one root node and has parent"
         , input       = [Record 0 1]
         , expected    = Nothing
         }
  , Case { description = "root node has parent"
         , input       = [Record 0 1, Record 1 0]
         , expected    = Nothing
         }
  , Case { description = "no root node"
         , input       = [Record 1 0, Record 2 0]
         , expected    = Nothing
         }
  , Case { description = "duplicate node"
         , input       = [Record 0 0, Record 1 0, Record 1 0]
         , expected    = Nothing
         }
  , Case { description = "duplicate root"
         , input       = [Record 0 0, Record 0 0]
         , expected    = Nothing
         }
  , Case { description = "non-continuous"
         , input       = [Record 2 0, Record 4 2, Record 1 0, Record 0 0]
         , expected    = Nothing
         }
  , Case { description = "cycle directly"
         , input       = [Record 5 2, Record 3 2, Record 2 2, Record 4 1, Record 1 0, Record 0 0, Record 6 3]
         , expected    = Nothing
         }
  , Case { description = "cycle indirectly"
         , input       = [Record 5 2, Record 3 2, Record 2 6, Record 4 1, Record 1 0, Record 0 0, Record 6 3]
         , expected    = Nothing
         }
  , Case { description = "higher id parent of lower id"
         , input       = [Record 0 0, Record 2 0, Record 1 2]
         , expected    = Nothing
         }
  ]
