module ArmstrongNumbers (armstrong) where

digits :: Integral x => x -> [x]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)

armstrong :: Integral a => a -> Bool
armstrong n = n == sum (map (^ length ds) ds)
  where
    ds = digits n
