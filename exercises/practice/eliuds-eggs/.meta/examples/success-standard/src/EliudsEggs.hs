module EliudsEggs (eggCount) where

eggCount :: Int -> Int
eggCount 0 = 0
eggCount x | odd x = 1 + eggCount (x `div` 2)
eggCount x = eggCount (x `div` 2)
