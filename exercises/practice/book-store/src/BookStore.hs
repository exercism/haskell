module BookStore (total, Book(..)) where

data Book = First | Second | Third | Fourth | Fifth

total :: [Book] -> Int
total basket = error "You need to implement this function."
