module Allergies (Allergen(..), allergies, isAllergicTo) where

data Allergen = Dummy

allergies :: Int -> [Allergen]
allergies = undefined

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo = undefined
