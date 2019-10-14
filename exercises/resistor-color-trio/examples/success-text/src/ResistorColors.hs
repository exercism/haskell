{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumDecimals #-}
module ResistorColors (Color(..), Resistor(..), label, ohms) where

import Data.Text (Text, pack)

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

label :: Resistor -> Text
label resistor
  | ohms' < kilo = tshow ohms'               <> " ohms"
  | ohms' < mega = tshow (ohms' `quot` kilo) <> " kiloohms"
  | ohms' < giga = tshow (ohms' `quot` mega) <> " megaohms"
  | otherwise    = tshow (ohms' `quot` giga) <> " gigaohms"
  where
    ohms' = ohms resistor

tshow :: Show a => a -> Text
tshow = pack . show

ohms :: Resistor -> Int
ohms (Resistor (a, b, e)) =
  (10 * fromEnum a + fromEnum b) * 10 ^ fromEnum e

kilo, mega, giga :: Int
kilo = 1e3
mega = 1e6
giga = 1e9
