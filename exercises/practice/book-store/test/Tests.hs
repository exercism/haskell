{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import BookStore (total, Book(..))

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "total" $ for_ cases test
  where
    test Case{..} = it description $ total basket `shouldBe` expected

data Case = Case { description :: String
                 , comment     :: String
                 , basket      :: [Book]
                 , expected    :: Int
                 }

cases :: [Case]
cases = [ Case { description = "Only a single book"
               , comment     = "Suggested grouping, [[1]]."
               , basket      = [First]
               , expected    = 800
               }
        , Case { description = "Two of the same book"
               , comment     = "Suggested grouping, [[2],[2]]."
               , basket      = [Second, Second]
               , expected    = 1600
               }
        , Case { description = "Empty basket"
               , comment     = "Suggested grouping, []."
               , basket      = []
               , expected    = 0
               }
        , Case { description = "Two different books"
               , comment     = "Suggested grouping, [[1,2]]."
               , basket      = [First, Second]
               , expected    = 1520
               }
        , Case { description = "Three different books"
               , comment     = "Suggested grouping, [[1,2,3]]."
               , basket      = [First, Second, Third]
               , expected    = 2160
               }
        , Case { description = "Four different books"
               , comment     = "Suggested grouping, [[1,2,3,4]]."
               , basket      = [First, Second, Third, Fourth]
               , expected    = 2560
               }
        , Case { description = "Five different books"
               , comment     = "Suggested grouping, [[1,2,3,4,5]]."
               , basket      = [First, Second, Third, Fourth, Fifth]
               , expected    = 3000
               }
        , Case { description = "Two groups of four is cheaper than group of five plus group of three"
               , comment     = "Suggested grouping, [[1,2,3,4],[1,2,3,5]]."
               , basket      = [First, First, Second, Second, Third, Third, Fourth, Fifth]
               , expected    = 5120
               }
        , Case { description = "Two groups of four is cheaper than groups of five and three"
               , comment     = "Suggested grouping, [[1,2,4,5],[1,3,4,5]]. This differs from the other 'two groups of four' test in that it will fail for solutions that add books to groups in the order in which they appear in the list."
               , basket      = [First, First, Second, Third, Fourth, Fourth, Fifth, Fifth]
               , expected    = 5120
               }
        , Case { description = "Group of four plus group of two is cheaper than two groups of three"
               , comment     = "Suggested grouping, [[1,2,3,4],[1,2]]."
               , basket      = [First, First, Second, Second, Third, Fourth]
               , expected    = 4080
               }
        , Case { description = "Two each of first four books and one copy each of rest"
               , comment     = "Suggested grouping, [[1,2,3,4,5],[1,2,3,4]]."
               , basket      = [First, First, Second, Second, Third, Third, Fourth, Fourth, Fifth]
               , expected    = 5560
               }
        , Case { description = "Two copies of each book"
               , comment     = "Suggested grouping, [[1,2,3,4,5],[1,2,3,4,5]]."
               , basket      = [First, First, Second, Second, Third, Third, Fourth, Fourth, Fifth, Fifth]
               , expected    = 6000
               }
        , Case { description = "Three copies of first book and two each of remaining"
               , comment     = "Suggested grouping, [[1,2,3,4,5],[1,2,3,4,5],[1]]."
               , basket      = [First, First, Second, Second, Third, Third, Fourth, Fourth, Fifth, Fifth, First]
               , expected    = 6800
               }
        , Case { description = "Three each of first two books and two each of remaining books"
               , comment     = "Suggested grouping, [[1,2,3,4,5],[1,2,3,4,5],[1,2]]."
               , basket      = [First, First, Second, Second, Third, Third, Fourth, Fourth, Fifth, Fifth, First, Second]
               , expected    = 7520
               }
        , Case { description = "Four groups of four are cheaper than two groups each of five and three"
               , comment     = "Suggested grouping, [[1,2,3,4],[1,2,3,5],[1,2,3,4],[1,2,3,5]]."
               , basket      = [First, First, Second, Second, Third, Third, Fourth, Fifth, First, First, Second, Second, Third, Third, Fourth, Fifth]
               , expected    = 10240
               }
        , Case { description = "Check that groups of four are created properly even when there are more groups of three than groups of five"
               , comment     = "Suggested grouping, [[1,2,3,4],[1,2,3,5],[1,2,3,4],[1,2,3,5],[1,2,3],[1,2,3]]."
               , basket      = [First, First, First, First, First, First, Second, Second, Second, Second, Second, Second, Third, Third, Third, Third, Third, Third, Fourth, Fourth, Fifth, Fifth]
               , expected    = 14560
               }
        , Case { description = "One group of one and four is cheaper than one group of two and three"
               , comment     = "Suggested grouping, [[1],[1,2,3,4]]."
               , basket      = [First, First, Second, Third, Fourth]
               , expected    = 3360
               }
        , Case { description = "One group of one and two plus three groups of four is cheaper than one group of each size"
               , comment     = "Suggested grouping, [[5],[5,4],[5,4,3,2],[5,4,3,2],[5,4,3,1]]."
               , basket      = [First, Second, Second, Third, Third, Third, Fourth, Fourth, Fourth, Fourth, Fifth, Fifth, Fifth, Fifth, Fifth]
               , expected    = 10000
               }
        ]
