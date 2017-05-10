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
                , ("negative rows",-1, [                                    ]) ]
