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
  deriving (Eq, Show, Enum, Bounded)

convert :: Color -> Int
convert Black = 0
convert Brown = 1
convert Red = 2
convert Orange = 3
convert Yellow = 4
convert Green = 5
convert Blue = 6
convert Violet = 7
convert Grey = 8
convert White = 9

value :: (Color, Color) -> Int
value (a, b) = 10 * convert a + convert b
