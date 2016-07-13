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

-- Implementation of a row-major matrix for any type, using Data.Vector.
--
-- No validation of input is required. Let it fail if the matrix is not
-- rectangular, invalid chars are encountered, etc.
--
-- shape is (rows, cols)
--
-- The task is to create the data type `Matrix`, with `Eq`
-- and `Show` instances, and implement the functions below.

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
