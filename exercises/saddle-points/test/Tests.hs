{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Array        (listArray)
import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Matrix (saddlePoints)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "saddlePoints" $ for_ cases test
  where

    test (description, xss, expected) = it description assertion
      where
        assertion = saddlePoints matrix `shouldBe` expected
        rows      = length xss
        columns   = length $ head xss
        matrix    = listArray ((1, 1), (rows, columns)) (concat xss)

    cases = [ ( "Example from README",
                [ [9, 8, 7]
                , [5, 3, 2]
                , [6, 6, 7] ], [(2, 1)] )

            , ( "empty matrix has none", [], [] )

            , ( "no saddle point",
                [ [1, 2, 3]
                , [3, 1, 2]
                , [2, 3, 1] ], [] )

            , ( "multiple saddle points in a column",
                [ [4, 5, 4]
                , [3, 5, 5]
                , [1, 5, 4] ], [ (1, 2)
                               , (2, 2)
                               , (3, 2) ] )

            , ( "multiple saddle points in a row",
                [ [6, 7, 8]
                , [5, 5, 5]
                , [7, 5, 6] ], [ (2, 1)
                               , (2, 2)
                               , (2, 3) ] )

            , ( "bottom-right corner",
                [ [8, 7, 9]
                , [6, 7, 6]
                , [3, 2, 5] ], [(3, 3)] )

            , ( "non-square matrix",
                [ [3, 1, 3]
                , [3, 2, 4] ], [ (1, 1)
                               , (1, 3) ] )

            , ( "Can identify that saddle points in a single column matrix are those with the minimum value",
                [ [2]
                , [1]
                , [4]
                , [1] ], [ (2, 1)
                         , (4, 1) ] )

            , ( "Can identify that saddle points in a single row matrix are those with the maximum value",
                [ [2, 5, 3, 5] ], [ (1, 2)
                                  , (1, 4) ] )
            ]
