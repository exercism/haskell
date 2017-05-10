{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Sublist (Sublist(Equal,Sublist,Superlist,Unequal), sublist)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
          describe "standard tests" $ for_ cases test
          describe "track specific tests" $ do

            let xs = replicate 1000 'x'

            it "compare larger equal lists" $
              sublist xs xs
              `shouldBe` Equal

            it "sublist early in huge list" $
              sublist [3, 4, 5] [1..1000000 :: Int]
              `shouldBe` Sublist

            it "huge sublist not in huge list" $
              sublist [10..1000001] [1..1000000 :: Int]
              `shouldBe` Unequal

            it "superlist early in huge list" $
              sublist [1..1000000] [3, 4, 5 :: Int]
              `shouldBe` Superlist
  where

    test Case{..} = it explanation assertion
      where
        assertion   = sublist listOne listTwo `shouldBe` expectation
        explanation = unwords [ "sublist"
                              , show listOne
                              , show listTwo
                              , "-"
                              , description ]

data Case = Case { description :: String
                 , listOne     :: [Integer]
                 , listTwo     :: [Integer]
                 , expectation :: Sublist
                 }

cases :: [Case]
cases = [ Case { description = "empty lists"
               , listOne     = []
               , listTwo     = []
               , expectation = Equal
               }
        , Case { description = "empty list within non empty list"
               , listOne     = []
               , listTwo     = [1, 2, 3]
               , expectation = Sublist
               }
        , Case { description = "non empty list contains empty list"
               , listOne     = [1, 2, 3]
               , listTwo     = []
               , expectation = Superlist
               }
        , Case { description = "list equals itself"
               , listOne     = [1, 2, 3]
               , listTwo     = [1, 2, 3]
               , expectation = Equal
               }
        , Case { description = "different lists"
               , listOne     = [1, 2, 3]
               , listTwo     = [2, 3, 4]
               , expectation = Unequal
               }
        , Case { description = "false start"
               , listOne     = [1, 2, 5]
               , listTwo     = [0, 1, 2, 3, 1, 2, 5, 6]
               , expectation = Sublist
               }
        , Case { description = "consecutive"
               , listOne     = [1, 1, 2]
               , listTwo     = [0, 1, 1, 1, 2, 1, 2]
               , expectation = Sublist
               }
        , Case { description = "sublist at start"
               , listOne     = [0, 1, 2]
               , listTwo     = [0, 1, 2, 3, 4, 5]
               , expectation = Sublist
               }
        , Case { description = "sublist in middle"
               , listOne     = [2, 3, 4]
               , listTwo     = [0, 1, 2, 3, 4, 5]
               , expectation = Sublist
               }
        , Case { description = "sublist at end"
               , listOne     = [3, 4, 5]
               , listTwo     = [0, 1, 2, 3, 4, 5]
               , expectation = Sublist
               }
        , Case { description = "at start of superlist"
               , listOne     = [0, 1, 2, 3, 4, 5]
               , listTwo     = [0, 1, 2]
               , expectation = Superlist
               }
        , Case { description = "in middle of superlist"
               , listOne     = [0, 1, 2, 3, 4, 5]
               , listTwo     = [2, 3]
               , expectation = Superlist
               }
        , Case { description = "at end of superlist"
               , listOne     = [0, 1, 2, 3, 4, 5]
               , listTwo     = [3, 4, 5]
               , expectation = Superlist
               }
        , Case { description = "first list missing element from second list"
               , listOne     = [1, 3]
               , listTwo     = [1, 2, 3]
               , expectation = Unequal
               }
        , Case { description = "second list missing element from first list"
               , listOne     = [1, 2, 3]
               , listTwo     = [1, 3]
               , expectation = Unequal
               }
        , Case { description = "order matters to a list"
               , listOne     = [1, 2, 3]
               , listTwo     = [3, 2, 1]
               , expectation = Unequal
               }
        , Case { description = "same digits but different numbers"
               , listOne     = [1, 0, 1]
               , listTwo     = [10, 1]
               , expectation = Unequal
               }
        ]
