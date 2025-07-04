import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import FlowerField (annotate)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "annotate" $ for_ cases test
  where

    test (description, board) = it description assertion
      where
        assertion  = annotate (clearBoard board) `shouldBe` board
        clearBoard = map (map flowerOrSpace)
        flowerOrSpace '*' = '*'
        flowerOrSpace  _  = ' '

    cases = [ ("no rows", [] )

            , ("no columns", [ "" ] )

            , ("no flowers", [ "   "
                             , "   "
                             , "   " ] )

            , ("board with only flowers", [ "***"
                                          , "***"
                                          , "***" ] )

            , ("flower surrounded by spaces", [ "111"
                                              , "1*1"
                                              , "111" ] )

            , ("space surrounded by flowers", [ "***"
                                              , "*8*"
                                              , "***" ] )

            , ("horizontal line", [ "1*2*1" ] )

            , ("horizontal line, flowers at edges", [ "*1 1*" ] )

            , ("vertical line", [ "1"
                                , "*"
                                , "2"
                                , "*"
                                , "1" ] )

            , ("vertical line, flowers at edges", [ "*"
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
