module CryptoSquare (encode) where

import Data.Char       (isAlphaNum, toLower)
import Data.List       (transpose)
import Data.List.Split (chunksOf)

encode :: String -> String
encode = unwords
       . justify
       . transpose
       . (squareSize >>= chunksOf)
       . map toLower
       . filter isAlphaNum
  where
    squareSize :: String -> Int
    squareSize = ceiling . (sqrt :: Double -> Double) . fromIntegral . length
    justify :: [String] -> [String]
    justify [] = []
    justify (x:xs) = let width = length x in
                         -- each row either needs zero or one spaces as padding,
                         -- so there's no need to write (s ++ repeat ' ')
                         x : map (\s -> take width (s ++ " ")) xs
