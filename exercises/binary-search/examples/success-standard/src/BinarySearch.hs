module BinarySearch (binarySearch) where
import Data.Array (Array, (!))
  
binFind ::  Array Int Int -> Int -> Int -> Int -> Int
binFind arr x lwr upr
  | middleVal == x = midInd
  | x > middleVal = binFind arr x (midInd + 1) upr
  | otherwise = binFind arr x lwr (midInd - 1)
  where midInd = abs (upr + lwr) `div` 2 
        middleVal = arr ! midInd

binarySearch :: Array Int Int -> Int -> Maybe Int
binarySearch arr x
  | null arr = Nothing
  | length arr == 1 = if arr ! 0 == x then Just 0 else Nothing
  | x < (arr ! 0) || x > (arr ! (length arr - 1)) = Nothing
  | otherwise = Just $ binFind arr x 0 $ length arr
