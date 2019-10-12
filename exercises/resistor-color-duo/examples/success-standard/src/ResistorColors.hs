module ResistorColors (Color(..), value) where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show)

value1 :: Color -> Int
value1 Black = 0
value1 Brown = 1
value1 Red = 2
value1 Orange = 3
value1 Yellow = 4
value1 Green = 5
value1 Blue = 6
value1 Violet = 7
value1 Grey = 8
value1 White = 9

value :: (Color, Color) -> Int
value (a, b) = 10 * value1 a + value1 b
