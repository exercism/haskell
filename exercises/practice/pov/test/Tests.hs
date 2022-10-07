{-# LANGUAGE TupleSections #-}

import Data.Foldable     (for_)
import Data.Function     (on)
import Data.Tree         (Tree(Node), rootLabel)
import Data.List         (sort)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import POV (fromPOV, tracePathBetween)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    describe "fromPOV" $ do

      let cases =
            [ ("reparenting singleton"        , singleton , Just singleton')
            , ("reparenting with sibling"     , simple    , Just simple'   )
            , ("reparenting flat"             , flat      , Just flat'     )
            , ("reparenting nested"           , nested    , Just nested'   )
            , ("reparenting kids"             , kids      , Just kids'     )
            , ("reparenting cousins"          , cousins   , Just cousins'  )
            , ("from POV of non-existent node", leaf "foo", Nothing        ) ]

          rootShouldMatch  = shouldBe `on` fmap rootLabel
          edgesShouldMatch = shouldBe `on` fmap (sort . toEdges)

          test (name, input, output) = describe name $ do
            it "correct root"  $ fromPOV "x" input `rootShouldMatch`  output
            it "correct edges" $ fromPOV "x" input `edgesShouldMatch` output

          in for_ cases test

      describe "Should not be able to find a missing node" $

        let cases = [ ("singleton", singleton)
                    , ("flat"     , flat     )
                    , ("kids"     , kids     )
                    , ("nested"   , nested   )
                    , ("cousins"  , cousins  ) ]

            test (name, g) = it name $ fromPOV "NOT THERE" g `shouldBe` Nothing

            in for_ cases test

    describe "tracePathBetween" $ do

      it "Can find path from x -> parent" $
        tracePathBetween "x" "parent" simple
        `shouldBe` Just [ "x"
                        , "parent" ]

      it "Can find path from x -> sibling" $
        tracePathBetween "x" "b" flat
        `shouldBe` Just [ "x"
                        , "root"
                        , "b"    ]

      it "Can trace a path from x -> cousin" $
        tracePathBetween "x" "cousin-1" cousins
        `shouldBe` Just [ "x"
                        , "parent"
                        , "grandparent"
                        , "uncle"
                        , "cousin-1"    ]

      it "Can find path from nodes other than x" $
        tracePathBetween "a" "c" flat
        `shouldBe` Just [ "a"
                        , "root"
                        , "c"    ]

      it "Can find path not involving root" $
        tracePathBetween "x" "sibling-1" rootNotNeeded
        `shouldBe` Just [ "x"
                        , "parent"
                        , "sibling-1" ]

      it "Cannot trace if destination does not exist" $
        tracePathBetween "x" "NOT THERE" cousins
        `shouldBe` Nothing

      it "Cannot trace if source does not exist" $
        tracePathBetween "NOT THERE" "x" cousins
        `shouldBe` Nothing

-- Functions used in the tests.

leaf :: a -> Tree a
leaf v = Node v []

-- In the trees we're making, we don't care about the ordering of children.
-- This is significant when rerooting on nodes that have a parent and children.
-- The former parent can go either before or after the former children.
-- Either choice would be correct in the context of this problem.
-- So all we need to check is:
-- 1) The graph is actually rooted on the requested node.
-- 2) The sorted edge list is correct.
-- This function helps check the second condition.

toEdges :: Ord a => Tree a -> [(a, a)]
toEdges (Node r ts) = map ((r,) . rootLabel) ts ++ concatMap toEdges ts

-- Trees used in the tests.

singleton , simple , flat , kids , nested , cousins  :: Tree String
singleton', simple', flat', kids', nested', cousins' :: Tree String

singleton = leaf "x"

singleton' = leaf "x"

simple = Node "parent"
             [ leaf "x"
             , leaf "sibling"
             ]

simple' = Node "x"
              [ Node "parent"
                    [ leaf "sibling"
                    ]
              ]

flat = Node "root"
           [ leaf "a"
           , leaf "b"
           , leaf "x"
           , leaf "c"
           ]

flat' = Node "x"
            [ Node "root"
                  [ leaf "a"
                  , leaf "b"
                  , leaf "c"
                  ]
            ]

kids = Node "root"
           [ Node "x"
                 [ leaf "kid-0"
                 , leaf "kid-1"
                 ]
           ]

kids' = Node "x"
            [ leaf "kid-0"
            , leaf "kid-1"
            , leaf "root"
            ]

nested = Node "level-0"
             [ Node "level-1"
                   [ Node "level-2"
                         [ Node "level-3"
                               [ leaf "x"
                               ]
                         ]
                   ]
             ]

nested' = Node "x"
              [ Node "level-3"
                    [ Node "level-2"
                          [ Node "level-1"
                                [ leaf "level-0"
                                ]
                          ]
                    ]
              ]

cousins = Node "grandparent"
              [ Node "parent"
                    [ Node "x"
                          [ leaf "kid-a"
                          , leaf "kid-b"
                          ]
                    , leaf "sibling-0"
                    , leaf "sibling-1"
                    ]
              , Node "uncle"
                    [ leaf "cousin-0"
                    , leaf "cousin-1"
                    ]
              ]

cousins' = Node "x"
               [ leaf "kid-a"
               , leaf "kid-b"
               , Node "parent"
                     [ leaf "sibling-0"
                     , leaf "sibling-1"
                     , Node "grandparent"
                           [ Node "uncle"
                                 [ leaf "cousin-0"
                                 , leaf "cousin-1"
                                 ]
                           ]
                     ]
               ]

rootNotNeeded :: Tree String
rootNotNeeded = Node "grandparent"
                    [ Node "parent"
                          [ leaf "x"
                          , leaf "sibling-0"
                          , leaf "sibling-1"
                          ]
                    ]
