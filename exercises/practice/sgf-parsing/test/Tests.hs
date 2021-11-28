{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable     (for_)
import Data.Map          (fromList)
import Data.Tree         (Tree(Node))
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Sgf (parseSgf)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "parseSgf" $ for_ cases test
  where

    test (input, expected) = it description assertion
      where
        description = unwords ["parse", show input]
        assertion   = parseSgf input `shouldBe` fmap fromList <$> expected

    cases = [ (""                                   , Nothing                                                )
            -- error: tree with no nodes
            , ("()"                                 , Nothing                                                )
            -- error: node without tree
            , (";"                                  , Nothing                                                )
            -- error: node without properties
            , ("(;)"                                , Just  $ Node [] []                                     )
            -- single-node tree
            , ("(;A[B])"                            , Just  $ Node [("A", ["B"])] []                         )
            -- multiple properties
            , ("(;A[b]C[d])"                        , Just  $ Node [("A", ["b"]), ("C", ["d"])] []           )
            -- error: properties without value
            , ("(;A)"                               , Nothing                                                )
            -- error: lowercase property identifier
            , ("(;a[b])"                            , Nothing                                                )
            -- error: mixed-case property identifier
            , ("(;Aa[b])"                           , Nothing                                                )
            -- two nodes
            , ("(;A[B];B[C])"                       , Just  $ Node [("A", ["B"])] [ Node [("B", ["C"])] [] ] )
            -- two child trees
            , ("(;A[B](;B[C])(;C[D]))"              , Just  $ Node [("A", ["B"])] [ Node [("B", ["C"])] []
                                                                                  , Node [("C", ["D"])] [] ] )
            -- multiple property values
            , ("(;A[b][c][d])"                      , Just  $ Node [("A", ["b", "c", "d" ])] []              )
            -- escaped property value
            , ("(;A[\\]b\nc\\\nd\t\te\\\\ \\\n\\]])", Just  $ Node [("A", ["]b cd  e\\ ]"])] []              ) ]

-- b74debc3be24b5c81650189935c9bbfa019b367e
