{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Array        (listArray)
import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Matrix (saddlePoints)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "saddle-points" $
          describe "saddlePoints" $ for_ cases test
  where

    test (description, xss, expected) = it description assertion
      where
        assertion = saddlePoints matrix `shouldBe` expected
        rows      = length xss
        columns   = length $ head xss
        matrix    = listArray ((0, 0), (rows - 1, columns - 1)) (concat xss)

    -- As of 2016-09-12, there was no reference file
    -- for the test cases in `exercism/x-common`.

    cases = [ ("Example from README", [ [9, 8, 7]
                                      , [5, 3, 2]
                                      , [6, 6, 7] ], [(1, 0)] )

            , ( "no saddle point", [ [2, 1]
                                   , [1, 2] ], [] )

            , ( "a saddle point", [ [1, 2]
                                  , [3, 4] ], [(0, 1)] )

            , ( "another saddle point", [ [18,  3, 39, 19,  91]
                                        , [38, 10,  8, 77, 320]
                                        , [ 3,  4,  8,  6,   7] ], [(2, 2)] )

            , ("multiple saddle points", [ [4, 5, 4]
                                         , [3, 5, 5]
                                         , [1, 5, 4] ], [ (0, 1)
                                                        , (1, 1)
                                                        , (2, 1) ] )
            ]
