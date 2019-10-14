{-# LANGUAGE NumDecimals #-}
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
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

label :: Resistor -> String
label resistor
  | ohms' < kilo = show ohms'               ++ " ohms"
  | ohms' < mega = show (ohms' `quot` kilo) ++ " kiloohms"
  | ohms' < giga = show (ohms' `quot` mega) ++ " megaohms"
  | otherwise    = show (ohms' `quot` giga) ++ " gigaohms"
  where
    ohms' = ohms resistor

ohms :: Resistor -> Int
ohms (Resistor (a, b, e)) =
  (10 * fromEnum a + fromEnum b) * 10 ^ fromEnum e

kilo, mega, giga :: Int
kilo = 1e3
mega = 1e6
giga = 1e9
