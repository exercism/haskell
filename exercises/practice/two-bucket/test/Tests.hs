{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Data.Tuple        (swap)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import TwoBucket (measure)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "measure" $ for_ cases test
  where
    test Case{..} = it description $ measure capacity target `shouldBe` expected
 
data Case = Case { description :: String
                 , capacity    :: (Int, Int)
                 , target      :: Int
                 , expected    :: Maybe (Int, (Int, Int))
                 }

cases :: [Case]
cases = [ Case { description = "Measure using bucket one of size 3 and bucket two of size 5 - start with bucket one"
               , capacity = (3, 5)
               , target = 1
               , expected = Just (4, (1, 5))
               }
        , Case { description = "Measure using bucket one of size 3 and bucket two of size 5 - start with bucket two"
               , capacity = swap (3, 5)
               , target = 1
               , expected = Just (8, swap (3, 1))
               }
        , Case { description = "Measure using bucket one of size 7 and bucket two of size 11 - start with bucket one"
               , capacity = (7, 11)
               , target = 2
               , expected = Just (14, (2, 11))
               }
        , Case { description = "Measure using bucket one of size 7 and bucket two of size 11 - start with bucket two"
               , capacity = swap (7, 11)
               , target = 2
               , expected = Just (18, swap (7, 2))
               }
        , Case { description = "Measure one step using bucket one of size 1 and bucket two of size 3 - start with bucket two"
               , capacity = swap (1, 3)
               , target = 3
               , expected = Just (1, swap (0, 3))
               }
        , Case { description = "Measure using bucket one of size 2 and bucket two of size 3 - start with bucket one and end with bucket two"
               , capacity = (2, 3)
               , target = 3
               , expected = Just (2, (2, 3))
               }
        , Case { description = "Not possible to reach the goal"
               , capacity = (6, 15)
               , target = 5
               , expected = Nothing
               }
        , Case { description = "With the same buckets but a different goal, then it is possible"
               , capacity = (6, 15)
               , target = 9
               , expected = Just (10, (0, 9))
               }
        , Case { description = "Goal larger than both buckets is impossible"
               , capacity = (5, 7)
               , target = 8
               , expected = Nothing
               }
        ]
