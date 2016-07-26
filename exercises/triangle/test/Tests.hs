{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Triangle (TriangleType(..), triangleType)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "triangle" $
          describe "triangleType" $ for_ cases test
  where

    test (a, b, c, expected) = it explanation assertion
      where
        explanation = unwords . map show $ [a, b, c]
        assertion   = triangleType a b c  `shouldBe` expected

    -- As of 2016-07-30, there was no reference file
    -- for the test cases in `exercism/x-common`.

    cases = [ ( 2,  2,  2, Equilateral)
            , (10, 10, 10, Equilateral)
            , ( 3,  4,  4, Isosceles  )
            , ( 4,  3,  4, Isosceles  )
            , ( 9, 12,  9, Isosceles  )
            , ( 3,  4,  5, Scalene    )
            , ( 1,  1, 50, Illogical  )
            , ( 1,  2,  1, Illogical  )
            , ( 0,  0,  0, Illogical  ) ]
