{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import TreeBuilding (newTree, Tree(..), Record(..))

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "tree-building" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion = newTree records `shouldBe` expected

data Case = Case { description :: String
                 , records     :: [Record]
                 , expected    :: Maybe Tree
                 }

cases :: [Case]
cases = [ Case { description = "Empty input"
               , records     = []
               , expected    = Nothing
               }
        , Case { description = "One Record"
               , records     = [ Record 0 Nothing ]
               , expected    = Just (Leaf 0)
               }
        , Case { description = "Three nodes in order"
               , records     = [ Record 0 Nothing
                               , Record 1 (Just 0)
                               , Record 2 (Just 0)
                               ]
               , expected    = Just (Branch 0 [ Leaf 1, Leaf 2 ])
               }
        , Case { description = "Three nodes in reverse order"
               , records     = [ Record 2 (Just 0)
                               , Record 1 (Just 0)
                               , Record 0 Nothing
                               ]
               , expected    = Just (Branch 0 [ Leaf 1, Leaf 2 ])
               }
        , Case { description = "More than two children"
               , records     = [ Record 3 (Just 0)
                               , Record 2 (Just 0)
                               , Record 1 (Just 0)
                               , Record 0 Nothing
                               ]
               , expected    = Just (Branch 0 [ Leaf 1
                                              , Leaf 2
                                              , Leaf 3 ])
               }
        , Case { description = "Binary tree"
               , records     = [ Record 5 (Just 1)
                               , Record 3 (Just 2)
                               , Record 2 (Just 0)
                               , Record 4 (Just 1)
                               , Record 1 (Just 0)
                               , Record 0 Nothing
                               , Record 6 (Just 2)
                               ]
               , expected    = Just (Branch 0 [ Branch 1 [ Leaf 4
                                                         , Leaf 5 ]
                                              , Branch 2 [ Leaf 3
                                                         , Leaf 6 ]
                                              ])
               }
        , Case { description = "Unbalanced tree"
               , records     = [ Record 5 (Just 2)
                               , Record 3 (Just 2)
                               , Record 2 (Just 0)
                               , Record 4 (Just 1)
                               , Record 1 (Just 0)
                               , Record 0 Nothing
                               , Record 6 (Just 2)
                               ]
               , expected    = Just (Branch 0 [ Branch 1 [ Leaf 4 ]
                                              , Branch 2 [ Leaf 3
                                                         , Leaf 5
                                                         , Leaf 6 ]
                                              ])
               }
        , Case { description = "Root has parent"
               , records     = [ Record 0 (Just 1)
                               , Record 1 (Just 0)
                               ]
               , expected    = Nothing
               }
        , Case { description = "No root"
               , records     = [ Record 1 (Just 0) ]
               , expected    = Nothing
               }
        , Case { description = "Duplicate"
               , records     = [ Record 0 Nothing
                               , Record 1 (Just 0)
                               , Record 1 (Just 0)
                               ]
               , expected    = Nothing
               }
        , Case { description = "Duplicate root"
               , records     = [ Record 0 Nothing, Record 0 Nothing ]
               , expected    = Nothing
               }
        , Case { description = "Non-continuous"
               , records     = [ Record 2 (Just 0)
                               , Record 4 (Just 2)
                               , Record 1 (Just 0)
                               , Record 0 Nothing
                               ]
               , expected    = Nothing
               }
        , Case { description = "Cycle Directly"
               , records     = [ Record 5 (Just 2)
                               , Record 3 (Just 2)
                               , Record 2 (Just 2)
                               , Record 4 (Just 1)
                               , Record 1 (Just 0)
                               , Record 0 Nothing
                               , Record 6 (Just 3)
                               ]
               , expected    = Nothing
               }
        , Case { description = "Cycle indirectly"
               , records     = [ Record 5 (Just 2)
                               , Record 3 (Just 2)
                               , Record 2 (Just 6)
                               , Record 4 (Just 1)
                               , Record 1 (Just 0)
                               , Record 0 Nothing
                               , Record 6 (Just 3)
                               ]
               , expected    = Nothing
               }
        , Case { description = "Higher id parent of lower id"
               , records     = [ Record 0 Nothing
                               , Record 2 (Just 0)
                               , Record 1 (Just 2)
                               ]
               , expected    = Nothing
               }
        ]
