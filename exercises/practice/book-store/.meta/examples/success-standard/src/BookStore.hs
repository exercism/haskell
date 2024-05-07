module BookStore (total, Book(..)) where

import Data.List ( group, sort )

data Book = First | Second | Third | Fourth | Fifth deriving (Eq, Ord, Show)

-- Reverse sort on count of each book first. I'm not sure if this work in every possible case.
-- Naive algorithm (generate solution candidates by forming groups) is too slow.

total :: [Book] -> Int
total basket = minPrice 5 $ reverse $ sort $ map length $ group $ sort basket
-- Reverse sort the numbers of each books

-- Find min price with a grouping of at most the given size for the given book counts
minPrice :: Int -> [Int] -> Int
minPrice _ [] = 0
minPrice 1 counts = sum $ map (\x -> price 1 * x) counts
minPrice n counts =
    if length counts < n
    then minPrice (n-1) counts
    else min (price n + minPrice n remainingCounts) (minPrice (n-1) counts)
      where
        remainingCounts = filter (/= 0) (map (\x -> x-1) $ take n counts) <> drop n counts

-- Get the price of a group containing given amount of books
price :: Int -> Int
price 1 =  800 --  0% discount
price 2 = 1520 --  5% discount
price 3 = 2160 -- 10% discount
price 4 = 2560 -- 20% discount
price 5 = 3000 -- 25% discount
price _ = 0