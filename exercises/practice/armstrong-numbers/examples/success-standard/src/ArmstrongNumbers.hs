module ArmstrongNumbers (armstrong) where

digits :: Integral x => x -> [x]
digits 0 = []
digits x = x `rem` 10 : digits (x `quot` 10)

armstrong :: Integral a => a -> Bool
armstrong n = n == sum (map (^ length ds) ds)
  where
    ds = digits n
