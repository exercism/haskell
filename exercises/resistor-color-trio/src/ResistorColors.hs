module ResistorColors (Color(..), Resistor(..), label, ohms) where

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

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving (Eq, Show)

label :: Resistor -> String
label resistor = error "You need to implement this function."

ohms :: Resistor -> Int
ohms resistor = error "You need to implement this function."
