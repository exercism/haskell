{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Data.Array (Array, listArray)

import BinarySearch (binarySearch)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "foundElement" $ for_ cases test
  where
    test Case{..} = it description assertion
      where
        assertion = binarySearch input value `shouldBe` expected


data Case a = Case { description :: String
                 , input       :: Array Int a
                 , value       :: a
                 , expected    :: Maybe Int
                 }
 
cases :: [Case Int]
cases = [ Case { description = "finds a value in an array with one element"
               , input       = listArray (0, 0) [6]
               , value       = 6
               , expected    = Just 0
               }
        , Case { description = "finds a value in the middle of an array"
               , input       = array1
               , value       = 6
               , expected    = Just 3
               }
        , Case { description = "finds a value at the beginning of an array"
               , input       = array1
               , value       = 1
               , expected    = Just 0
               }
        , Case { description = "finds a value at the end of an array"
               , input       = array1
               , value       = 11
               , expected    = Just 6
               }
        , Case { description = "finds a value in an array of odd length"
               , input       = listArray (0, 12) [1, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 634]
               , value       = 144
               , expected    = Just 9
               }
        , Case { description = "finds a value in an array of even length"
               , input       = listArray (0, 11) [1, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]
               , value       = 21
               , expected    = Just 5
               }
        , Case { description = "a value smaller than the array's smallest value is not included"
               , input       = array1
               , value       = 0
               , expected    = Nothing
               }
        , Case { description = "a value larger than the array's largest value is not included"
               , input       = array1
               , value       = 13
               , expected    = Nothing
               }
        , Case { description = "nothing is included in an empty array"
               , input       = listArray (1, 0) [] -- There is no way to represent an empty array
               , value       = 1
               , expected    = Nothing
               }
        ]
        where array1 = listArray (0, 6) [1, 3, 4, 6, 8, 9, 11]
