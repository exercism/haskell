module KitchenCalculator (getVolume, toMilliliter, fromMilliliter, convert, Messurments (..)) where

data Messurments = Cup | FluidOnce | TableSpoon | TeaSpoon | Milliliter deriving (Eq, Show)

getVolume :: (Messurments, Double) -> Double
getVolume (_, n) = n

toMilliliter :: (Messurments, Double) -> (Messurments, Double)
toMilliliter (Cup, n) = (Milliliter, n * 240)
toMilliliter (FluidOnce, n) = (Milliliter, n * 30)
toMilliliter (TableSpoon, n) = (Milliliter, n * 15)
toMilliliter (TeaSpoon, n) = (Milliliter, n * 5)
toMilliliter valuePair = valuePair


fromMilliliter :: (Messurments, Double) -> Messurments -> (Messurments, Double)
fromMilliliter (_, n) Milliliter = (Milliliter, n)
fromMilliliter (_, n) Cup = (Cup, n / 240)
fromMilliliter (_, n) FluidOnce = (FluidOnce, n / 30)
fromMilliliter (_, n) TableSpoon = (TableSpoon, n / 15)
fromMilliliter (_, n) TeaSpoon = (TeaSpoon, n / 5)

convert :: (Messurments, Double) -> Messurments -> (Messurments, Double)
convert (m, n) to = fromMilliliter (toMilliliter (m, n)) to
