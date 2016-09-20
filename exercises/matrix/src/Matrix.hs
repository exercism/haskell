module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Data.Vector (Vector)

cols :: Matrix a -> Int
cols = undefined

column :: Int -> Matrix a -> Vector a
column = undefined

flatten :: Matrix a -> Vector a
flatten = undefined

fromList :: [[a]] -> Matrix a
fromList = undefined

fromString :: Read a => String -> Matrix a
fromString = undefined

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape = undefined

row :: Int -> Matrix a -> Vector a
row = undefined

rows :: Matrix a -> Int
rows = undefined

shape :: Matrix a -> (Int, Int)
shape = undefined

transpose :: Matrix a -> Matrix a
transpose = undefined
