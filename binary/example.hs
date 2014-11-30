{-# LANGUAGE BangPatterns #-}
module Binary (toDecimal) where
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)

toDecimal :: String -> Int
toDecimal = fromMaybe 0 . foldM go 0
  where
    go !n c = (n * 2 +) `fmap` toDigit c
    toDigit '0' = Just 0
    toDigit '1' = Just 1
    toDigit _   = Nothing
