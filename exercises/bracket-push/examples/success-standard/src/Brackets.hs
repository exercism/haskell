module Brackets (arePaired) where

data BracketType = Opening | Closing
data Stack a = Empty | Elem a (Stack a)

push :: Char -> Stack Char -> Stack Char
push = Elem

pop :: Stack Char -> Stack Char
pop Empty = Empty
pop (Elem _ stack) = stack

arePaired :: String -> Bool
arePaired xs = checkBalance xs Empty

checkBalance :: String -> Stack Char -> Bool
checkBalance [] Empty = True
checkBalance [] _ = False
checkBalance (x:xs) stack =
  case classify x of
    Just Opening -> checkBalance xs $ push x stack
    Just Closing -> (x `closes` stack) && checkBalance xs (pop stack)
    _            -> checkBalance xs stack

classify :: Char -> Maybe BracketType
classify x
  | x `elem` "([{" = Just Opening
  | x `elem` ")]}" = Just Closing
  | otherwise      = Nothing

closes :: Char -> Stack Char -> Bool
closes _ Empty = False
closes x (Elem y _) =
  x == ')' && y == '('
  || x == ']' && y == '['
  || x == '}' && y == '{'
