{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable     (for_)
import Data.Map          (fromList)
import Data.Tree         (Tree(Node))
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Sgf (parseSgf)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

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
            -- within property values, whitespace characters such as tab are converted to spaces
            , ("(;A[hello\t\tworld])"               , Just  $ Node [("A", ["hello  world"])] []              )
            -- within property values, newlines remain as newlines
            , ("(;A[hello\n\nworld])"               , Just  $ Node [("A", ["hello\n\nworld"])] []            )
            -- escaped closing bracket within property value becomes just a closing bracket
            , ("(;A[\\]])"                          , Just  $ Node [("A", ["]"])] []                         )
            -- escaped backslash in property value becomes just a backslash
            , ("(;A[\\\\])"                         , Just  $ Node [("A", ["\\"])] []                        )
            -- opening bracket within property value doesn't need to be escaped
            , ("(;A[x[y\\]z][foo]B[bar];C[baz])"    , Just  $ Node [("A", ["x[y]z", "foo"]), ("B", ["bar"])] [ Node [("C", ["baz"])] [] ] )
            -- semicolon in property value doesn't need to be escaped
            , ("(;A[a;b][foo]B[bar];C[baz])"        , Just  $ Node [("A", ["a;b", "foo"]), ("B", ["bar"])] [ Node [("C", ["baz"])] [] ] )
            -- parentheses in property value don't need to be escaped
            , ("(;A[x(y)z][foo]B[bar];C[baz])"      , Just  $ Node [("A", ["x(y)z", "foo"]), ("B", ["bar"])] [ Node [("C", ["baz"])] [] ] )
            -- escaped tab in property value is converted to space
            , ("(;A[hello\\\tworld])"               , Just  $ Node [("A", ["hello world"])] []               )
            -- escaped newline in property value is converted to nothing at all
            , ("(;A[hello\\\nworld])"               , Just  $ Node [("A", ["helloworld"])] []                )
            -- escaped t and n in property value are just letters, not whitespace
            , ("(;A[\\t = t and \\n = n])"          , Just  $ Node [("A", ["t = t and n = n"])] []           )
            -- mixing various kinds of whitespace and escaped characters in property value
            , ("(;A[\\]b\nc\\\nd\t\te\\\\ \\\n\\]])", Just  $ Node [("A", ["]b\ncd  e\\ ]"])] []             ) ]
