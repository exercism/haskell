module Allergies (Allergen(..), allergies, isAllergicTo) where

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq)

allergies :: Int -> [Allergen]
allergies = error "You need to implement this function."

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen n = error "You need to implement this function."
