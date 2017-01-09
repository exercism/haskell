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
allergies = undefined

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo = undefined
