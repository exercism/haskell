{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Triangle (rows)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "rows" $ for_ rowsCases rowsTest
  where

    rowsTest (description, n, expected) = it description assertion
      where
        assertion = rows n `shouldBe` expected

    rowsCases = [ ("no rows"      , 0, [                                    ])
                , ("single row"   , 1, [[1]                                 ])
                , ("two rows"     , 2, [[1], [1, 1]                         ])
                , ("three rows"   , 3, [[1], [1, 1], [1, 2, 1]              ])
                , ("four rows"    , 4, [[1], [1, 1], [1, 2, 1], [1, 3, 3, 1]])
                , ("five rows"    , 5, [[1],
                                        [1, 1],
                                        [1, 2, 1],
                                        [1, 3, 3, 1],
                                        [1, 4, 6, 4, 1]])
                , ("six rows"     , 6, [[1],
                                        [1, 1],
                                        [1, 2, 1],
                                        [1, 3, 3, 1],
                                        [1, 4, 6, 4, 1],
                                        [1, 5, 10, 10, 5, 1]])
                , ("ten rows"     ,10, [[1],
                                        [1, 1],
                                        [1, 2, 1],
                                        [1, 3, 3, 1],
                                        [1, 4, 6, 4, 1],
                                        [1, 5, 10, 10, 5, 1],
                                        [1, 6, 15, 20, 15, 6, 1],
                                        [1, 7, 21, 35, 35, 21, 7, 1],
                                        [1, 8, 28, 56, 70, 56, 28, 8, 1],
                                        [1, 9, 36, 84, 126, 126, 84, 36, 9, 1]])
                ]
