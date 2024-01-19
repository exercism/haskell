{-# LANGUAGE RecordWildCards #-}

import Prelude hiding    (abs, div, Rational)
import Data.Function     (on)
import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import RationalNumbers
  ( Rational
  , abs
  , numerator
  , denominator
  , add
  , sub
  , mul
  , div
  , pow
  , expRational
  , expReal
  , rational
  )

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
          describe "rational"    $ for_ rationalCases    $ testU rational
          describe "numerator"   $ for_ numeratorCases   $ testU numerator
          describe "denominator" $ for_ denominatorCases $ testU denominator
          describe "abs"         $ for_ absCases         $ testU abs
          describe "add"         $ for_ addCases         $ testB add
          describe "sub"         $ for_ subCases         $ testB sub
          describe "mul"         $ for_ mulCases         $ testB mul
          describe "div"         $ for_ divCases         $ testB div
          describe "pow"         $ for_ powCases         $ testB pow
          describe "expRational" $ for_ expRationalCases $ testR expRational
          describe "expReal"     $ for_ expRealCases     $ testR expReal
 where
  testU f CaseU{..} = it descriptionU $ f number1U `shouldBe` expectedU
  testB f CaseB{..} = it descriptionB $ f number1B number2B `shouldBe` expectedB
  testR f CaseB{..} = it descriptionB $ f number1B number2B `shouldBeAround` expectedB
  shouldBeAround = shouldBe `on` roundTo 2

  roundTo :: Int -> Float -> Float
  roundTo n = (/ 10 ^ n) . (fromInteger :: Integer -> Float) . round . (* 10 ^ n)

data CaseU a b = CaseU { descriptionU :: String
                       , number1U     :: a
                       , expectedU    :: b
                       }

data CaseB a b c = CaseB { descriptionB :: String
                         , number1B     :: a
                         , number2B     :: b
                         , expectedB    :: c
                         }

rationalCases :: [CaseU (Integer, Integer) (Rational Integer)]
rationalCases =
    [ CaseU { descriptionU = "Reduce a positive rational number to lowest terms"
            , number1U     = (2, 4)
            , expectedU    = rational (1, 2)
            }
    , CaseU { descriptionU = "Reduce places the minus sign on the numerator"
            , number1U     = (3, -4)
            , expectedU    = rational (-3, 4)
            }
    , CaseU { descriptionU = "Reduce a negative rational number to lowest terms"
            , number1U     = (-4, 6)
            , expectedU    = rational (-2, 3)
            }
    , CaseU { descriptionU = "Reduce a rational number with a negative denominator to lowest terms"
            , number1U     = (3, -9)
            , expectedU    = rational (-1, 3)
            }
    , CaseU { descriptionU = "Reduce zero to lowest terms"
            , number1U     = (0, 6)
            , expectedU    = rational (0, 1)
            }
    , CaseU { descriptionU = "Reduce an integer to lowest terms"
            , number1U     = (-14, 7)
            , expectedU    = rational (-2, 1)
            }
    , CaseU { descriptionU = "Reduce one to lowest terms"
            , number1U     = (13, 13)
            , expectedU    = rational (1, 1)
            }
    ]

numeratorCases :: [CaseU (Rational Integer) Integer]
numeratorCases =
    [ CaseU { descriptionU = "Numerator of a reduced rational number"
            , number1U     = rational (3, -4)
            , expectedU    = -3
            }
    ]

denominatorCases :: [CaseU (Rational Integer) Integer]
denominatorCases =
    [ CaseU { descriptionU = "Denominator of a reduced rational number"
            , number1U     = rational (3, -4)
            , expectedU    = 4
            }
    ]

absCases :: [CaseU (Rational Integer) (Rational Integer)]
absCases =
    [ CaseU { descriptionU = "Absolute value of a positive rational number"
            , number1U     = rational (1, 2)
            , expectedU    = rational (1, 2)
            }
    , CaseU { descriptionU = "Absolute value of a positive rational number with negative numerator and denominator"
            , number1U     = rational (-1, -2)
            , expectedU    = rational (1, 2)
            }
    , CaseU { descriptionU = "Absolute value of a negative rational number"
            , number1U     = rational (-1, 2)
            , expectedU    = rational (1, 2)
            }
    , CaseU { descriptionU = "Absolute value of a negative rational number with negative denominator"
            , number1U     = rational (1, -2)
            , expectedU    = rational (1, 2)
            }
    , CaseU { descriptionU = "Absolute value of zero"
            , number1U     = rational (0, 1)
            , expectedU    = rational (0, 1)
            }
    , CaseU { descriptionU = "Absolute value of a rational number is reduced to lowest terms"
            , number1U     = rational (2, 4)
            , expectedU    = rational (1, 2)
            }
    ]

addCases :: [CaseB (Rational Integer) (Rational Integer) (Rational Integer)]
addCases =
    [ CaseB { descriptionB = "Add two positive rational numbers"
            , number1B     = rational (1, 2)
            , number2B     = rational (2, 3)
            , expectedB    = rational (7, 6)
            }
    , CaseB { descriptionB = "Add a positive rational number and a negative rational number"
            , number1B     = rational (1, 2)
            , number2B     = rational (-2, 3)
            , expectedB    = rational (-1, 6)
            }
    , CaseB { descriptionB = "Add two negative rational numbers"
            , number1B     = rational (-1, 2)
            , number2B     = rational (-2, 3)
            , expectedB    = rational (-7, 6)
            }
    , CaseB { descriptionB = "Add a rational number to its additive inverse"
            , number1B     = rational (1, 2)
            , number2B     = rational (-1, 2)
            , expectedB    = rational (0, 1)
            }
    ]

subCases :: [CaseB (Rational Integer) (Rational Integer) (Rational Integer)]
subCases =
    [ CaseB { descriptionB = "Subtract two positive rational numbers"
            , number1B     = rational (1, 2)
            , number2B     = rational (2, 3)
            , expectedB    = rational (-1, 6)
            }
    , CaseB { descriptionB = "Subtract a positive rational number and a negative rational number"
            , number1B     = rational (1, 2)
            , number2B     = rational (-2, 3)
            , expectedB    = rational (7, 6)
            }
    , CaseB { descriptionB = "Subtract two negative rational numbers"
            , number1B     = rational (-1, 2)
            , number2B     = rational (-2, 3)
            , expectedB    = rational (1, 6)
            }
    , CaseB { descriptionB = "Subtract a rational number from itself"
            , number1B     = rational (1, 2)
            , number2B     = rational (1, 2)
            , expectedB    = rational (0, 1)
            }
    ]

mulCases :: [CaseB (Rational Integer) (Rational Integer) (Rational Integer)]
mulCases =
    [ CaseB { descriptionB = "Multiply two positive rational numbers"
            , number1B     = rational (1, 2)
            , number2B     = rational (2, 3)
            , expectedB    = rational (1, 3)
            }
    , CaseB { descriptionB = "Multiply a negative rational number by a positive rational number"
            , number1B     = rational (-1, 2)
            , number2B     = rational (2, 3)
            , expectedB    = rational (-1, 3)
            }
    , CaseB { descriptionB = "Multiply two negative rational numbers"
            , number1B     = rational (-1, 2)
            , number2B     = rational (-2, 3)
            , expectedB    = rational (1, 3)
            }
    , CaseB { descriptionB = "Multiply a rational number by its reciprocal"
            , number1B     = rational (1, 2)
            , number2B     = rational (2, 1)
            , expectedB    = rational (1, 1)
            }
    , CaseB { descriptionB = "Multiply a rational number by 1"
            , number1B     = rational (1, 2)
            , number2B     = rational (1, 1)
            , expectedB    = rational (1, 2)
            }
    , CaseB { descriptionB = "Multiply a rational number by 0"
            , number1B     = rational (1, 2)
            , number2B     = rational (0, 1)
            , expectedB    = rational (0, 1)
            }
    ]

divCases :: [CaseB (Rational Integer) (Rational Integer) (Rational Integer)]
divCases =
    [ CaseB { descriptionB = "Divide two positive rational numbers"
            , number1B     = rational (1, 2)
            , number2B     = rational (2, 3)
            , expectedB    = rational (3, 4)
            }
    , CaseB { descriptionB = "Divide a positive rational number by a negative rational number"
            , number1B     = rational (1, 2)
            , number2B     = rational (-2, 3)
            , expectedB    = rational (-3, 4)
            }
    , CaseB { descriptionB = "Divide two negative rational numbers"
            , number1B     = rational (-1, 2)
            , number2B     = rational (-2, 3)
            , expectedB    = rational (3, 4)
            }
    , CaseB { descriptionB = "Divide a rational number by 1"
            , number1B     = rational (1, 2)
            , number2B     = rational (1, 1)
            , expectedB    = rational (1, 2)
            }
    ]

powCases :: [CaseB (Rational Integer) Integer (Rational Integer)]
powCases =
    [ CaseB { descriptionB = "Raise a positive rational number to a positive integer power"
            , number1B     = rational (1, 2)
            , number2B     = 3
            , expectedB    = rational (1, 8)
            }
    , CaseB { descriptionB = "Raise a negative rational number to a positive integer power"
            , number1B     = rational (-1, 2)
            , number2B     = 3
            , expectedB    = rational (-1, 8)
            }
    , CaseB { descriptionB = "Raise a positive rational number to a negative integer power"
            , number1B     = rational (3, 5)
            , number2B     = -2
            , expectedB    = rational (25, 9)
            }
    , CaseB { descriptionB = "Raise a negative rational number to an even negative integer power"
            , number1B     = rational (-3, 5)
            , number2B     = -2
            , expectedB    = rational (25, 9)
            }
    , CaseB { descriptionB = "Raise a negative rational number to an odd negative integer power"
            , number1B     = rational (-3, 5)
            , number2B     = -3
            , expectedB    = rational (-125, 27)
            }
    , CaseB { descriptionB = "Raise zero to an integer power"
            , number1B     = rational (0, 1)
            , number2B     = 5
            , expectedB    = rational (0, 1)
            }
    , CaseB { descriptionB = "Raise one to an integer power"
            , number1B     = rational (1, 1)
            , number2B     = 4
            , expectedB    = rational (1, 1)
            }
    , CaseB { descriptionB = "Raise a positive rational number to the power of zero"
            , number1B     = rational (1, 2)
            , number2B     = 0
            , expectedB    = rational (1, 1)
            }
    , CaseB { descriptionB = "Raise a negative rational number to the power of zero"
            , number1B     = rational (-1, 2)
            , number2B     = 0
            , expectedB    = rational (1, 1)
            }
    ]

expRationalCases :: [CaseB (Rational Integer) Float Float]
expRationalCases =
    [ CaseB { descriptionB = "Raise a rational number to a positive real number bigger than 1"
            , number1B     = rational (1, 2)
            , number2B     = 3
            , expectedB    = 0.125
            }
    , CaseB { descriptionB = "Raise a rational number to a positive real number smaller than 1"
            , number1B     = rational (1, 2)
            , number2B     = 0.5
            , expectedB    = 0.707
            }
    , CaseB { descriptionB = "Raise a rational number to a negative real number bigger than 1"
            , number1B     = rational (1, 2)
            , number2B     = -3
            , expectedB    = 8.0
            }
    , CaseB { descriptionB = "Raise a rational number to a negative real number smaller than 1"
            , number1B     = rational (1, 2)
            , number2B     = -0.5
            , expectedB    = 1.41
            }
    , CaseB { descriptionB = "Raise a rational number to a zero real number"
            , number1B     = rational (4, 3)
            , number2B     = 0
            , expectedB    = 1.0
            }
    ]

expRealCases :: [CaseB Float (Rational Integer) Float]
expRealCases =
    [ CaseB { descriptionB = "Raise a real number to a positive rational number"
            , number1B     = 8
            , number2B     = rational (4, 3)
            , expectedB    = 16.0
            }
    , CaseB { descriptionB = "Raise a real number to a negative rational number"
            , number1B     = 9
            , number2B     = rational (-1, 2)
            , expectedB    = 0.3333333333333333
            }
    , CaseB { descriptionB = "Raise a real number to a zero rational number"
            , number1B     = 2
            , number2B     = rational (0, 1)
            , expectedB    = 1.0
            }
    ]
