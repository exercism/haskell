{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Triangle
  ( TriangleType ( Equilateral
                 , Illegal
                 , Isosceles
                 , Scalene
                 )
  , triangleType
  )

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "triangleType" $ for_ cases test
  where

    test (description, (a, b, c), expected) = it description assertion
      where
        assertion = triangleType a b c `shouldBe` expected

    cases = [ ( "equilateral triangle has all sides equal"
              , (2, 2, 2)
              , Equilateral
              )
            , ( "larger equilateral triangle"
              , (10, 10, 10)
              , Equilateral
              )
            , ( "isosceles triangle with last two sides equal"
              , (3, 4, 4)
              , Isosceles
              )
            , ( "isosceles triangle with first two sides equal"
              , (4, 4, 3)
              , Isosceles
              )
            , ( "isosceles triangle with first and last sides equal"
              , (4, 3, 4)
              , Isosceles
              )
            , ( "isosceles triangle with unequal side larger than equal sides"
              , (4, 7, 4)
              , Isosceles
              )
            , ( "scalene triangle has no equal sides"
              , (3, 4, 5)
              , Scalene
              )
            , ( "2a == b+c looks like equilateral, but isn't always"
              , (5, 4, 6)
              , Scalene
              )
            , ( "larger scalene triangle"
              , (10, 11, 12)
              , Scalene
              )
            , ( "scalene triangle with sides in descending order"
              , (5, 4, 2)
              , Scalene
              )
            , ( "small scalene triangle with floating point values"
              , (0.4, 0.6, 0.3)
              , Scalene
              )
            , ( "a triangle violating the triangle inequality is illegal"
              , (7, 3, 2)
              , Illegal
              )
            , ( "two sides equal, but still violates triangle inequality"
              , (1, 1, 3)
              , Illegal
              )
            , ( "triangles with all sides zero are illegal"
              , (0, 0, 0)
              , Illegal
              )
            ]
