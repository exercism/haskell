module Change (findFewestCoins) where

import Data.List (sortBy)

type Coin = Integer
type Coins = [Coin]
type Amount = Integer

findFewestCoins :: Amount -> Coins -> Maybe Coins
findFewestCoins target coins = minChange target sortedCoins [] Nothing
  where
    sortedCoins = sortBy (flip compare) coins

    minChange target coins candidate bestResult
      | target < 0 || worseResult = bestResult
      | target == 0 = Just candidate
      | otherwise = dropCoin target coins candidate newBestResult
      where
        worseResult = maybe False (\x -> length x <= length candidate) bestResult
        newBestResult = addCoin target coins candidate bestResult

    addCoin target coins@(coin:_) candidate bestResult
      | newTarget >= 0 = minChange newTarget coins (coin:candidate) bestResult
      where newTarget = target - coin
    addCoin _ _ _ bestResult = bestResult

    dropCoin target (_:restCoins) candidate bestResult =
      minChange target restCoins candidate bestResult
    dropCoin _ _ _ bestResult = bestResult
