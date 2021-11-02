module Strain (keep, discard) where

keep, discard :: (a -> Bool) -> [a] -> [a]

keep p = keep' []
  where keep' acc [] = reverse acc
        keep' acc (x:xs) = keep' (if p x then x:acc else acc) xs

discard = keep . (not .)
