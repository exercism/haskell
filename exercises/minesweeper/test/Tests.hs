import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Minesweeper (annotate)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "annotate" $ for_ cases test
  where

    test (description, board) = it description assertion
      where
        assertion  = annotate (clearBoard board) `shouldBe` board
        clearBoard = map (map mineOrSpace)
        mineOrSpace '*' = '*'
        mineOrSpace  _  = ' '

    cases = [ ("no rows", [] )

            , ("no columns", [ "" ] )

            , ("no mines", [ "   "
                           , "   "
                           , "   " ] )

            , ("board with only mines", [ "***"
                                        , "***"
                                        , "***" ] )

            , ("mine surrounded by spaces", [ "111"
                                            , "1*1"
                                            , "111" ] )

            , ("space surrounded by mines", [ "***"
                                            , "*8*"
                                            , "***" ] )

            , ("horizontal line", [ "1*2*1" ] )

            , ("horizontal line, mines at edges", [ "*1 1*" ] )

            , ("vertical line", [ "1"
                                , "*"
                                , "2"
                                , "*"
                                , "1" ] )

            , ("vertical line, mines at edges", [ "*"
                                                , "1"
                                                , " "
                                                , "1"
                                                , "*" ] )

            , ("cross", [ " 2*2 "
                        , "25*52"
                        , "*****"
                        , "25*52"
                        , " 2*2 " ] )

            , ("large board", [ "1*22*1"
                              , "12*322"
                              , " 123*2"
                              , "112*4*"
                              , "1*22*2"
                              , "111111" ] )
            ]
