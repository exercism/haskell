import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Minesweeper (annotate)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "minesweeper" $
          describe "annotate" $ for_ cases test
  where

    -- As of 2016-08-09, there was no reference file
    -- for the test cases in `exercism/x-common`.

    test (description, board) = it description assertion
      where
        assertion  = annotate (clearBoard board) `shouldBe` board
        clearBoard = map (map mineOrSpace)
        mineOrSpace '*' = '*'
        mineOrSpace  _  = ' '

    cases = [ ("zero size board" , [] )

            , ("empty board" , [ "   "
                               , "   "
                               , "   " ] )

            , ("board full of mines" , [ "***"
                                       , "***"
                                       , "***" ] )

            , ("surrounded", [ "***"
                             , "*8*"
                             , "***" ] )

            , ("horizontal line", [ "1*2*1" ] )

            , ("vertical line", [ "1"
                                , "*"
                                , "2"
                                , "*"
                                , "1" ] )

            , ("cross", [ " 2*2 "
                        , "25*52"
                        , "*****"
                        , "25*52"
                        , " 2*2 " ] )
            ]
