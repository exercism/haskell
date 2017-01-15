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

data Matrix a = Dummy deriving (Eq, Show)

cols :: Matrix a -> Int
cols = error "You need to implement this function."

column :: Int -> Matrix a -> Vector a
column = error "You need to implement this function."

flatten :: Matrix a -> Vector a
flatten = error "You need to implement this function."

fromList :: [[a]] -> Matrix a
fromList = error "You need to implement this function."

fromString :: Read a => String -> Matrix a
fromString = error "You need to implement this function."

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape = error "You need to implement this function."

row :: Int -> Matrix a -> Vector a
row = error "You need to implement this function."

rows :: Matrix a -> Int
rows = error "You need to implement this function."

shape :: Matrix a -> (Int, Int)
shape = error "You need to implement this function."

transpose :: Matrix a -> Matrix a
transpose = error "You need to implement this function."
