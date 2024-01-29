module Knapsack (maximumValue) where

type Item = (Int, Int)
type Items = [Item]

maximumValue :: Int -> Items -> Int
maximumValue n items = maximum $ fst <$> solve n items

solve :: Int -> Items -> [(Int, Items)]
solve _ [] = [(0, [])]
solve limit (item@(w,v):rest) = if w > limit then withoutItem else withoutItem <> withItem
    where
        withoutItem = solve limit rest
        withItem = prependItem <$> solve (limit-w) rest
        prependItem (val, lst) = (val + v, item:lst)
