module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illogical
                  deriving (Eq, Show)

triangleType = undefined
