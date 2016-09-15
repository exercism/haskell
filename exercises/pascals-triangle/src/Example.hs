module Triangle (rows) where

rows :: Integral a => Int -> [[a]]
rows = flip take triangle

triangle :: Integral a => [[a]]
triangle = iterate (\x -> zipWith (+) (0 : x) (x ++ [0])) [1]
