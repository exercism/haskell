module LanguageList (new, add, remove, first, count, isFunctionalList) where

new :: [String]
new = []

add :: String -> [String]  -> [String]
add language list = language : list

remove :: [String] -> [String]
remove list = tail list

first :: [String] -> String
first list = head list

count :: [String] -> Int
count list = length list

isFunctionalList :: [String] -> Bool
isFunctionalList list = "Haskell" `elem` list
