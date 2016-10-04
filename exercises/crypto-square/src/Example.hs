module CryptoSquare (encode) where

import Data.Char       (isAlphaNum, toLower)
import Data.List       (transpose)
import Data.List.Split (chunksOf)

encode :: String -> String
encode = unwords
       . transpose
       . (squareSize >>= chunksOf)
       . map toLower
       . filter isAlphaNum
  where
    squareSize :: String -> Int
    squareSize = ceiling . (sqrt :: Double -> Double) . fromIntegral . length
