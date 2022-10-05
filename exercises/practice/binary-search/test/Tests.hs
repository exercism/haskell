{-# LANGUAGE RecordWildCards #-}

import Data.Array        (Array, listArray)
import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import BinarySearch      (find)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "find" $ for_ bases $ for_ cases . test
  where
    -- Data.Array supports arbitrary indexing. Does your implementation?
    bases = [ 0 {- , 1, -9 -} ]

    test base Case{..} = it (description ++ " (base " ++ show base ++ ")") $
      find (fromListWithBase base array) value `shouldBe` (+ base) <$> expected

fromListWithBase :: Int -> [a] -> Array Int a
fromListWithBase base xs = listArray (base, length xs - 1 + base) xs

data Case = Case { description :: String
                 , array       :: [Int]
                 , value       :: Int
                 , expected    :: Maybe Int
                 }

cases :: [Case]
cases =
  [ Case { description = "finds a value in an array with one element"
         , array       = [6]
         , value       = 6
         , expected    = Just 0
         }
  , Case { description = "finds a value in the middle of an array"
         , array       = [1, 3, 4, 6, 8, 9, 11]
         , value       = 6
         , expected    = Just 3
         }
  , Case { description = "finds a value at the beginning of an array"
         , array       = [1, 3, 4, 6, 8, 9, 11]
         , value       = 1
         , expected    = Just 0
         }
  , Case { description = "finds a value at the end of an array"
         , array       = [1, 3, 4, 6, 8, 9, 11]
         , value       = 11
         , expected    = Just 6
         }
  , Case { description = "finds a value in an array of odd length"
         , array       = [1, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 634]
         , value       = 144
         , expected    = Just 9
         }
  , Case { description = "finds a value in an array of even length"
         , array       = [1, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]
         , value       = 21
         , expected    = Just 5
         }
  , Case { description = "identifies that a value is not included in the array"
         , array       = [1, 3, 4, 6, 8, 9, 11]
         , value       = 7
         , expected    = Nothing
         }
  , Case { description = "a value smaller than the array's smallest value is not found"
         , array       = [1, 3, 4, 6, 8, 9, 11]
         , value       = 0
         , expected    = Nothing
         }
  , Case { description = "a value larger than the array's largest value is not found"
         , array       = [1, 3, 4, 6, 8, 9, 11]
         , value       = 13
         , expected    = Nothing
         }
  , Case { description = "nothing is found in an empty array"
         , array       = []
         , value       = 1
         , expected    = Nothing
         }
  , Case { description = "nothing is found when the left and right bounds cross"
         , array       = [1, 2]
         , value       = 0
         , expected    = Nothing
         }
  ]
