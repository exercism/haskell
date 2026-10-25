module KitchenCalculator (getVolume, toMilliliter, fromMilliliter, convert, Messurments (..)) where

data Messurments = Cup | FluidOnce | TableSpoon | TeaSpoon | Milliliter deriving (Eq, Show)

getVolume :: (Messurments, Double) -> Double
getVolume = error "Implement this function."

toMilliliter :: (Messurments, Double) -> (Messurments, Double)
toMilliliter = error "Implement this function."

fromMilliliter :: (Messurments, Double) -> Messurments -> (Messurments, Double)
fromMilliliter = error "Implement this function."

convert :: (Messurments, Double) -> Messurments -> (Messurments, Double)
convert = error "Implement this function."
