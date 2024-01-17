{-# LANGUAGE RecordWildCards #-}

import Prelude hiding    (abs, div, Rational)
import qualified Prelude as P
import Data.Function     (on)
import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import RationalNumbers
  ( Rational
  , abs
  , reduce
  , add
  , sub
  , mul
  , div
  , exprational
  , expreal
  , rational
  )

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
          describe "abs"         $ for_ absCases         $ testA abs
          describe "reduce"      $ for_ reduceCases      $ testA reduce
          describe "add"         $ for_ addCases         $ testB add
          describe "sub"         $ for_ subCases         $ testB sub
          describe "mul"         $ for_ mulCases         $ testB mul
          describe "div"         $ for_ divCases         $ testB div
          describe "exprational" $ for_ exprationalCases $ testC exprational
          describe "expreal"     $ for_ exprealCases     $ testD expreal
 where
  testA f CaseA{..} = it descriptionA $ f (rational number1A)
                                        `shouldBe` rational expectedA
  testB f CaseB{..} = it descriptionB $ f (rational number1B) (rational number2B)
                                        `shouldBe` rational expectedB
  testC f CaseC{..} = it descriptionC $ f (rational number1C) number2C
                                        `shouldBe` rational expectedC
  testD f CaseD{..} = it descriptionD $ f number1D (rational number2D)
                                        `shouldBeAround` expectedD
  shouldBeAround = shouldBe `on` roundTo 2

  roundTo :: Int -> Float -> Float
  roundTo n = (/ 10 ^ n) . (fromInteger :: Integer -> Float) . round . (* 10 ^ n)

data CaseA = CaseA { descriptionA :: String
                   , number1A     :: (Integer, Integer)
                   , expectedA    :: (Integer, Integer)
                   }

data CaseB = CaseB { descriptionB :: String
                   , number1B     :: (Integer, Integer)
                   , number2B     :: (Integer, Integer)
                   , expectedB    :: (Integer, Integer)
                   }

data CaseC = CaseC { descriptionC :: String
                   , number1C     :: (Integer, Integer)
                   , number2C     :: Integer
                   , expectedC    :: (Integer, Integer)
                   }

data CaseD = CaseD { descriptionD :: String
                   , number1D     :: Float
                   , number2D     :: (Integer, Integer)
                   , expectedD    :: Float
                   }

absCases :: [CaseA]
absCases =
    [ CaseA { descriptionA = "Absolute value of a positive rational number"
            , number1A     = (1, 2)
            , expectedA    = (1, 2)
            }
    , CaseA { descriptionA = "Absolute value of a positive rational number with negative numerator and denominator"
            , number1A     = (-1, -2)
            , expectedA    = (1, 2)
            }
    , CaseA { descriptionA = "Absolute value of a negative rational number"
            , number1A     = (-1, 2)
            , expectedA    = (1, 2)
            }
    , CaseA { descriptionA = "Absolute value of a negative rational number with negative denominator"
            , number1A     = (1, -2)
            , expectedA    = (1, 2)
            }
    , CaseA { descriptionA = "Absolute value of zero"
            , number1A     = (0, 1)
            , expectedA    = (0, 1)
            }
    , CaseA { descriptionA = "Absolute value of a rational number is reduced to lowest terms"
            , number1A     = (2, 4)
            , expectedA    = (1, 2)
            }
    ]

reduceCases :: [CaseA]
reduceCases =
    [ CaseA { descriptionA = "Reduce a positive rational number to lowest terms"
            , number1A     = (2, 4)
            , expectedA    = (1, 2)
            }
    , CaseA { descriptionA = "Reduce places the minus sign on the numerator"
            , number1A     = (3, -4)
            , expectedA    = (-3, 4)
            }
    , CaseA { descriptionA = "Reduce a negative rational number to lowest terms"
            , number1A     = (-4, 6)
            , expectedA    = (-2, 3)
            }
    , CaseA { descriptionA = "Reduce a rational number with a negative denominator to lowest terms"
            , number1A     = (3, -9)
            , expectedA    = (-1, 3)
            }
    , CaseA { descriptionA = "Reduce zero to lowest terms"
            , number1A     = (0, 6)
            , expectedA    = (0, 1)
            }
    , CaseA { descriptionA = "Reduce an integer to lowest terms"
            , number1A     = (-14, 7)
            , expectedA    = (-2, 1)
            }
    , CaseA { descriptionA = "Reduce one to lowest terms"
            , number1A     = (13, 13)
            , expectedA    = (1, 1)
            }
    ]

addCases :: [CaseB]
addCases =
    [ CaseB { descriptionB = "Add two positive rational numbers"
            , number1B     = (1, 2)
            , number2B     = (2, 3)
            , expectedB    = (7, 6)
            }
    , CaseB { descriptionB = "Add a positive rational number and a negative rational number"
            , number1B     = (1, 2)
            , number2B     = (-2, 3)
            , expectedB    = (-1, 6)
            }
    , CaseB { descriptionB = "Add two negative rational numbers"
            , number1B     = (-1, 2)
            , number2B     = (-2, 3)
            , expectedB    = (-7, 6)
            }
    , CaseB { descriptionB = "Add a rational number to its additive inverse"
            , number1B     = (1, 2)
            , number2B     = (-1, 2)
            , expectedB    = (0, 1)
            }
    ]

subCases :: [CaseB]
subCases =
    [ CaseB { descriptionB = "Subtract two positive rational numbers"
            , number1B     = (1, 2)
            , number2B     = (2, 3)
            , expectedB    = (-1, 6)
            }
    , CaseB { descriptionB = "Subtract a positive rational number and a negative rational number"
            , number1B     = (1, 2)
            , number2B     = (-2, 3)
            , expectedB    = (7, 6)
            }
    , CaseB { descriptionB = "Subtract two negative rational numbers"
            , number1B     = (-1, 2)
            , number2B     = (-2, 3)
            , expectedB    = (1, 6)
            }
    , CaseB { descriptionB = "Subtract a rational number from itself"
            , number1B     = (1, 2)
            , number2B     = (1, 2)
            , expectedB    = (0, 1)
            }
    ]

mulCases :: [CaseB]
mulCases =
    [ CaseB { descriptionB = "Multiply two positive rational numbers"
            , number1B     = (1, 2)
            , number2B     = (2, 3)
            , expectedB    = (1, 3)
            }
    , CaseB { descriptionB = "Multiply a negative rational number by a positive rational number"
            , number1B     = (-1, 2)
            , number2B     = (2, 3)
            , expectedB    = (-1, 3)
            }
    , CaseB { descriptionB = "Multiply two negative rational numbers"
            , number1B     = (-1, 2)
            , number2B     = (-2, 3)
            , expectedB    = (1, 3)
            }
    , CaseB { descriptionB = "Multiply a rational number by its reciprocal"
            , number1B     = (1, 2)
            , number2B     = (2, 1)
            , expectedB    = (1, 1)
            }
    , CaseB { descriptionB = "Multiply a rational number by 1"
            , number1B     = (1, 2)
            , number2B     = (1, 1)
            , expectedB    = (1, 2)
            }
    , CaseB { descriptionB = "Multiply a rational number by 0"
            , number1B     = (1, 2)
            , number2B     = (0, 1)
            , expectedB    = (0, 1)
            }
    ]

divCases :: [CaseB]
divCases =
    [ CaseB { descriptionB = "Divide two positive rational numbers"
            , number1B     = (1, 2)
            , number2B     = (2, 3)
            , expectedB    = (3, 4)
            }
    , CaseB { descriptionB = "Divide a positive rational number by a negative rational number"
            , number1B     = (1, 2)
            , number2B     = (-2, 3)
            , expectedB    = (-3, 4)
            }
    , CaseB { descriptionB = "Divide two negative rational numbers"
            , number1B     = (-1, 2)
            , number2B     = (-2, 3)
            , expectedB    = (3, 4)
            }
    , CaseB { descriptionB = "Divide a rational number by 1"
            , number1B     = (1, 2)
            , number2B     = (1, 1)
            , expectedB    = (1, 2)
            }
    ]

exprationalCases :: [CaseC]
exprationalCases =
    [ CaseC { descriptionC = "Raise a positive rational number to a positive integer power"
            , number1C     = (1, 2)
            , number2C     = 3
            , expectedC    = (1, 8)
            }
    , CaseC { descriptionC = "Raise a negative rational number to a positive integer power"
            , number1C     = (-1, 2)
            , number2C     = 3
            , expectedC    = (-1, 8)
            }
    , CaseC { descriptionC = "Raise a positive rational number to a negative integer power"
            , number1C     = (3, 5)
            , number2C     = -2
            , expectedC    = (25, 9)
            }
    , CaseC { descriptionC = "Raise a negative rational number to an even negative integer power"
            , number1C     = (-3, 5)
            , number2C     = -2
            , expectedC    = (25, 9)
            }
    , CaseC { descriptionC = "Raise a negative rational number to an odd negative integer power"
            , number1C     = (-3, 5)
            , number2C     = -3
            , expectedC    = (-125, 27)
            }
    , CaseC { descriptionC = "Raise zero to an integer power"
            , number1C     = (0, 1)
            , number2C     = 5
            , expectedC    = (0, 1)
            }
    , CaseC { descriptionC = "Raise one to an integer power"
            , number1C     = (1, 1)
            , number2C     = 4
            , expectedC    = (1, 1)
            }
    , CaseC { descriptionC = "Raise a positive rational number to the power of zero"
            , number1C     = (1, 2)
            , number2C     = 0
            , expectedC    = (1, 1)
            }
    , CaseC { descriptionC = "Raise a negative rational number to the power of zero"
            , number1C     = (-1, 2)
            , number2C     = 0
            , expectedC    = (1, 1)
            }
    ]

exprealCases :: [CaseD]
exprealCases =
    [ CaseD { descriptionD = "Raise a real number to a positive rational number"
            , number1D     = 8
            , number2D     = (4, 3)
            , expectedD    = 16.0
            }
    , CaseD { descriptionD = "Raise a real number to a negative rational number"
            , number1D     = 9
            , number2D     = (-1, 2)
            , expectedD    = 0.3333333333333333
            }
    , CaseD { descriptionD = "Raise a real number to a zero rational number"
            , number1D     = 2
            , number2D     = (0, 1)
            , expectedD    = 1.0
            }
    ]
