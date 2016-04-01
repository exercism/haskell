module Series (largestProduct) where
import Data.Char (digitToInt)
import Data.List (tails)

digits :: Integral a => String -> [a]
digits = map $ fromIntegral . digitToInt

slices :: Integral a => Int -> String -> [[a]]
slices n = go . digits
  where go xs = map (take n) $ take (length xs - pred n) (tails xs)

largestProduct :: Integral a => Int -> String -> Maybe a
largestProduct n text = case map product (slices n text) of
  []       -> Nothing
  products -> Just $ maximum products
