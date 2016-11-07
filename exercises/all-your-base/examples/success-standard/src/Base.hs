module Base (rebase) where

import Control.Monad (foldM)
import Data.List     (unfoldr)
import Data.Tuple    (swap)

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase ibase obase ds
    | ibase < 2 = Nothing
    | obase < 2 = Nothing
    | otherwise = toDigits obase <$> fromDigits ibase ds
  where

    fromDigits base = foldM f 0
      where
        f acc x | x >= 0 && x < base = Just (acc * base + x)
                | otherwise          = Nothing

    toDigits base = reverse . unfoldr f
      where
        f 0 = Nothing
        f x = Just . swap $ x `divMod` base
