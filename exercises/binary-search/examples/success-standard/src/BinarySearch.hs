module BinarySearch (binarySearch) where
import Data.Array (Array, (!), elems, listArray)
import Data.Maybe (isJust, fromJust)
  
binarySearch :: (Eq a, Ord a) => Array Int a -> a -> Maybe Int
binarySearch arr x
  | null arr = Nothing
  | length arr == 1 = if arr ! 0 == x then Just 0 else Nothing
  | x < (arr ! 0) || x > (arr ! (length arr - 1)) = Nothing
  | otherwise = let contents = elems arr
                    middle = length contents `div` 2
                    (farr, sarr) = splitAt middle contents
                    fhalf = listArray (0, length farr - 1) farr
                    shalf = listArray (0, length sarr - 1) sarr
                    target = if x >= head sarr then shalf else fhalf
                    res = binarySearch target x in 
                if isJust res && target == shalf 
                then Just ((fromJust res) + middle) else res
