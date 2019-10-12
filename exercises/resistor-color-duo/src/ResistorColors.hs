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

value :: (Color, Color) -> Int
value (a, b) = error "You need to implement this function."
