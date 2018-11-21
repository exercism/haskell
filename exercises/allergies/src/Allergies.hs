module Allergies (Allergen(..), allergies, isAllergicTo) where

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show)

allergies :: Int -> [Allergen]
allergies score = error "You need to implement this function."

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = error "You need to implement this function."
