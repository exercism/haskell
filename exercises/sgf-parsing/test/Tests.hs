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
            , ("()"                                 , Nothing                                                )
            , (";"                                  , Nothing                                                )
            , ("(;)"                                , Just  $ Node [] []                                     )
            , ("(;A[B])"                            , Just  $ Node [("A", ["B"])] []                         )
            , ("(;a)"                               , Nothing                                                )
            , ("(;a[b])"                            , Nothing                                                )
            , ("(;Aa[b])"                           , Nothing                                                )
            , ("(;A[B];B[C])"                       , Just  $ Node [("A", ["B"])] [ Node [("B", ["C"])] [] ] )
            , ("(;A[B](;B[C])(;C[D]))"              , Just  $ Node [("A", ["B"])] [ Node [("B", ["C"])] []
                                                                                  , Node [("C", ["D"])] [] ] )
            , ("(;A[b][c][d])"                      , Just  $ Node [("A", ["b", "c", "d" ])] []              )
            , ("(;A[\\]b\nc\\\nd\t\te\\\\ \\\n\\]])", Just  $ Node [("A", ["]b cd  e\\ ]"])] []              ) ]
