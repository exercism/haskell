{-# LANGUAGE RecordWildCards #-}

import Prelude hiding    (abs, div, exp)
import qualified Prelude as P
import Data.Function     (on)
import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import ComplexNumbers
  ( Complex
  , conjugate
  , abs
  , exp
  , real
  , imaginary
  , mul
  , add
  , sub
  , div
  , complex
  )

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
          describe "real"      $ for_ realCases      $ testC real
          describe "imaginary" $ for_ imaginaryCases $ testC imaginary
          describe "conjugate" $ for_ conjugateCases $ testB conjugate
          describe "abs"       $ for_ absCases       $ testC abs
          describe "exp"       $ for_ expCases       $ testB exp
          describe "mul"       $ for_ mulCases       $ testA mul
          describe "div"       $ for_ divCases       $ testA div
          describe "add"       $ for_ addCases       $ testA add
          describe "sub"       $ for_ subCases       $ testA sub
 where
  testA f CaseA{..} = it descriptionA $ f (complex number1A) (complex number2A)
                                        `shouldBeAround` complex expectedA
  testB f CaseB{..} = it descriptionB $ f (complex number1B)
                                        `shouldBeAround` complex expectedB
  testC f CaseC{..} = it descriptionC $ f (complex number1C)
                                        `shouldBe` expectedC
  shouldBeAround = shouldBe `on` roundComplex 2

  roundComplex :: Int -> Complex Float -> Complex Float
  roundComplex n c = complex (roundTo n (real c), roundTo n (imaginary c))

  roundTo :: Int -> Float -> Float
  roundTo n = (/ 10 ^ n) . (fromIntegral :: Integer -> Float) . round . (* 10 ^ n)

data CaseA = CaseA { descriptionA :: String
                   , number1A     :: (Float, Float)
                   , number2A     :: (Float, Float)
                   , expectedA    :: (Float, Float)
                   }

data CaseB = CaseB { descriptionB :: String
                   , number1B     :: (Float, Float)
                   , expectedB    :: (Float, Float)
                   }

data CaseC = CaseC { descriptionC :: String
                   , number1C     :: (Float, Float)
                   , expectedC    :: Float
                   }

realCases :: [CaseC]
realCases =
    [ CaseC { descriptionC = "Real part of a purely real number"
            , number1C     = (1, 0)
            , expectedC    = 1
            }
    , CaseC { descriptionC = "Real part of a purely imaginary number"
            , number1C     = (0, 1)
            , expectedC    = 0
            }
    , CaseC { descriptionC = "Real part of a number with real and imaginary part"
            , number1C     = (1, 2)
            , expectedC    = 1
            }
    ]

imaginaryCases :: [CaseC]
imaginaryCases =
    [ CaseC { descriptionC = "Imaginary part of a purely real number"
            , number1C     = (1, 0)
            , expectedC    = 0
            }
    , CaseC { descriptionC = "Imaginary part of a purely imaginary number"
            , number1C     = (0, 1)
            , expectedC    = 1
            }
    , CaseC { descriptionC = "Imaginary part of a number with real and imaginary part"
            , number1C     = (1, 2)
            , expectedC    = 2
            }
    ]

conjugateCases :: [CaseB]
conjugateCases =
    [ CaseB { descriptionB = "Conjugate a purely real number"
            , number1B     = (5, 0)
            , expectedB    = (5, 0)
            }
    , CaseB { descriptionB = "Conjugate a purely imaginary number"
            , number1B     = (0, 5)
            , expectedB    = (0, -5)
            }
    , CaseB { descriptionB = "Conjugate a number with real and imaginary part"
            , number1B     = (1, 1)
            , expectedB    = (1, -1)
            }
    ]

absCases :: [CaseC]
absCases =
    [ CaseC { descriptionC = "Absolute value of a positive purely real number"
            , number1C     = (5, 0)
            , expectedC    = 5
            }
    , CaseC { descriptionC = "Absolute value of a negative purely real number"
            , number1C     = (-5, 0)
            , expectedC    = 5
            }
    , CaseC { descriptionC = "Absolute value of a purely imaginary number with positive imaginary part"
            , number1C     = (0, 5)
            , expectedC    = 5
            }
    , CaseC { descriptionC = "Absolute value of a purely imaginary number with negative imaginary part"
            , number1C     = (0, -5)
            , expectedC    = 5
            }
    , CaseC { descriptionC = "Absolute value of a number with real and imaginary part"
            , number1C     = (3, 4)
            , expectedC    = 5
            }
    ]

mulCases :: [CaseA]
mulCases =
    [ CaseA { descriptionA = "Multiply purely real numbers"
            , number1A     = (1, 0)
            , number2A     = (2, 0)
            , expectedA    = (2, 0)
            }
    , CaseA { descriptionA = "Multiply purely imaginary numbers"
            , number1A     = (0, 1)
            , number2A     = (0, 2)
            , expectedA    = (-2, 0)
            }
    , CaseA { descriptionA = "Multiply numbers with real and imaginary part"
            , number1A     = (1, 2)
            , number2A     = (3, 4)
            , expectedA    = (-5, 10)
            }
    ]

divCases :: [CaseA]
divCases =
    [ CaseA { descriptionA = "Divide purely real numbers"
            , number1A     = (1, 0)
            , number2A     = (2, 0)
            , expectedA    = (0.5, 0)
            }
    , CaseA { descriptionA = "Divide purely imaginary numbers"
            , number1A     = (0, 1)
            , number2A     = (0, 2)
            , expectedA    = (0.5, 0)
            }
    , CaseA { descriptionA = "Divide numbers with real and imaginary part"
            , number1A     = (1, 2)
            , number2A     = (3, 4)
            , expectedA    = (0.44, 0.08)
            }
    ]

addCases :: [CaseA]
addCases =
    [ CaseA { descriptionA = "Add purely real numbers"
            , number1A     = (1, 0)
            , number2A     = (2, 0)
            , expectedA    = (3, 0)
            }
    , CaseA { descriptionA = "Add purely imaginary numbers"
            , number1A     = (0, 1)
            , number2A     = (0, 2)
            , expectedA    = (0, 3)
            }
    , CaseA { descriptionA = "Add numbers with real and imaginary part"
            , number1A     = (1, 2)
            , number2A     = (3, 4)
            , expectedA    = (4, 6)
            }
    ]

subCases :: [CaseA]
subCases =
    [ CaseA { descriptionA = "Subtract purely real numbers"
            , number1A     = (1, 0)
            , number2A     = (2, 0)
            , expectedA    = (-1, 0)
            }
    , CaseA { descriptionA = "Subtract purely imaginary numbers"
            , number1A     = (0, 1)
            , number2A     = (0, 2)
            , expectedA    = (0, -1)
            }
    , CaseA { descriptionA = "Subtract numbers with real and imaginary part"
            , number1A     = (1, 2)
            , number2A     = (3, 4)
            , expectedA    = (-2, -2)
            }
    ]

expCases :: [CaseB]
expCases =
    [ CaseB { descriptionB = "Euler's identity/formula"
            , number1B     = (0, pi)
            , expectedB    = (-1, 0)
            }
    , CaseB { descriptionB = "Exponential of 0"
            , number1B     = (0, 0)
            , expectedB    = (1, 0)
            }
    , CaseB { descriptionB = "Exponential of a purely real number"
            , number1B     = (1, 0)
            , expectedB    = (P.exp 1, 0)
            }
    , CaseB { descriptionB = "Exponential of a purely real number"
            , number1B     = (log 2, pi)
            , expectedB    = (-2, 0)
            }
    ]
