module CryptoSquare ( normalizePlaintext
                    , squareSize
                    , plaintextSegments
                    , ciphertext
                    , normalizeCiphertext ) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)
import Data.List.Split (chunksOf)

squareSize :: String -> Int
squareSize = ceiling . (sqrt :: Double -> Double) . fromIntegral . length

normalizePlaintext :: String -> String
normalizePlaintext = map toLower . filter isAlphaNum

plaintextSegments :: String -> [String]
plaintextSegments = (squareSize >>= chunksOf) . normalizePlaintext

ciphertext :: String -> String
ciphertext = concat . transpose . plaintextSegments

normalizeCiphertext :: String -> String
normalizeCiphertext = unwords . plaintextSegments . ciphertext
