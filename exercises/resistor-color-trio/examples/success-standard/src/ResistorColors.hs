{-# LANGUAGE NumDecimals #-}
module ResistorColors (Color(..), Resistor(..), label, ohms) where

import Data.Ratio (Ratio, numerator, denominator)

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
  | ohms' < kilo = showOhms ohms'          ++ " ohms"
  | ohms' < mega = showOhms (ohms' / kilo) ++ " kiloohms"
  | ohms' < giga = showOhms (ohms' / mega) ++ " megaohms"
  | otherwise    = showOhms (ohms' / giga) ++ " gigaohms"
  where
    ohms' = realToFrac (ohms resistor)

showOhms :: Ratio Int -> String
showOhms ohms' =
  if denominator ohms' == 1
  then show (numerator ohms')
  else show (realToFrac ohms' :: Double)

ohms :: Resistor -> Int
ohms (Resistor (a, b, e)) =
  (10 * fromEnum a + fromEnum b) * 10 ^ fromEnum e

kilo, mega, giga :: Num n => n
kilo = 1e3
mega = 1e6
giga = 1e9
