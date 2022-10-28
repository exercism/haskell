module Base (Error(..), rebase) where

import Control.Monad (foldM)
import Data.List     (unfoldr)
import Data.Tuple    (swap)

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase ibase obase ds
    | ibase < 2 = Left InvalidInputBase
    | obase < 2 = Left InvalidOutputBase
    | otherwise = toDigits obase <$> fromDigits ibase ds
  where

    fromDigits base = foldM f 0
      where
        f acc x | x >= 0 && x < base = Right (acc * base + x)
                | otherwise          = Left (InvalidDigit x)

    toDigits base = reverse . unfoldr f
      where
        f 0 = Nothing
        f x = Just . swap $ x `divMod` base
