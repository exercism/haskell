module Triangle (TriangleType(..), triangleType) where
import qualified Data.Set as S
import Data.List (sort)

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Read, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | any (<=0) [a, b, c] || s1 + s2 <= s3 = Illegal
  | otherwise = [Equilateral, Isosceles, Scalene] !! pred uniqueSides
  where (s1, s2, s3) = case sort [a, b, c] of
                         [s1', s2', s3'] -> (s1', s2', s3')
                         _ -> error "unreachable"
        uniqueSides  = S.size (S.fromAscList [s1, s2, s3])
