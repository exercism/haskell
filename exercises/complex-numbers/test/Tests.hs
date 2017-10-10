{-# LANGUAGE RecordWildCards #-}

import Prelude hiding    (abs, div)
import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import ComplexNumbers

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
          describe "real"      $ for_ realCases      $ testC real
          describe "imaginary" $ for_ imaginaryCases $ testC imaginary
          describe "conjugate" $ for_ conjugateCases $ testB conjugate
          describe "abs"       $ for_ absCases       $ testC abs
          describe "mul"       $ for_ mulCases       $ testA mul
          describe "div"       $ for_ divCases       $ testA div
          describe "add"       $ for_ addCases       $ testA add
          describe "sub"       $ for_ subCases       $ testA sub
 where
  testA f CaseA{..} = it descriptionA $ f number1A number2A `shouldBe` expectedA
  testB f CaseB{..} = it descriptionB $ f number1B `shouldBe` expectedB
  testC f CaseC{..} = it descriptionC $ f number1C `shouldBe` fromInteger expectedC


data CaseA = CaseA { descriptionA :: String
                   , number1A     :: Complex
                   , number2A     :: Complex
                   , expectedA    :: Complex
                   }

data CaseB = CaseB { descriptionB :: String
                   , number1B     :: Complex
                   , expectedB    :: Complex
                   }

data CaseC = CaseC { descriptionC :: String
                   , number1C     :: Complex
                   , expectedC    :: Integer
                   }

realCases :: [CaseC]
realCases =
    [ CaseC { descriptionC = "Real part of a purely real number"
            , number1C     = Complex 1 0
            , expectedC    = 1
            }
    , CaseC { descriptionC = "Real part of a purely imaginary number"
            , number1C     = Complex 0 1
            , expectedC    = 0
            }
    , CaseC { descriptionC = "Real part of a number with real and imaginary part"
            , number1C     = Complex 1 2
            , expectedC    = 1
            }
    ]

imaginaryCases :: [CaseC]
imaginaryCases =
    [ CaseC { descriptionC = "Imaginary part of a purely real number"
            , number1C     = Complex 1 0
            , expectedC    = 0
            }
    , CaseC { descriptionC = "Imaginary part of a purely imaginary number"
            , number1C     = Complex 0 1
            , expectedC    = 1
            }
    , CaseC { descriptionC = "Imaginary part of a number with real and imaginary part"
            , number1C     = Complex 1 2
            , expectedC    = 2
            }
    ]

conjugateCases :: [CaseB]
conjugateCases =
    [ CaseB { descriptionB = "Conjugate a purely real number"
            , number1B     = Complex 5 0
            , expectedB    = Complex 5 0
            }
    , CaseB { descriptionB = "Conjugate a purely imaginary number"
            , number1B     = Complex 0 5
            , expectedB    = Complex 0 (-5)
            }
    , CaseB { descriptionB = "Conjugate a number with real and imaginary part"
            , number1B     = Complex 1 1
            , expectedB    = Complex 1 (-1)
            }
    ]

absCases :: [CaseC]
absCases =
    [ CaseC { descriptionC = "Absolute value of a positive purely real number"
            , number1C     = Complex 5 0
            , expectedC    = 5
            }
    , CaseC { descriptionC = "Absolute value of a negative purely real number"
            , number1C     = Complex (-5) 0
            , expectedC    = 5
            }
    , CaseC { descriptionC = "Absolute value of a purely imaginary number with positive imaginary part"
            , number1C     = Complex 0 5
            , expectedC    = 5
            }
    , CaseC { descriptionC = "Absolute value of a purely imaginary number with negative imaginary part"
            , number1C     = Complex 0 (-5)
            , expectedC    = 5
            }
    , CaseC { descriptionC = "Absolute value of a number with real and imaginary part"
            , number1C     = Complex 3 4
            , expectedC    = 5
            }
    ]

mulCases :: [CaseA]
mulCases =
    [ CaseA { descriptionA = "Multiply purely real numbers"
            , number1A     = Complex 1 0
            , number2A     = Complex 2 0
            , expectedA    = Complex 2 0
            }
    , CaseA { descriptionA = "Multiply purely imaginary numbers"
            , number1A     = Complex 0 1
            , number2A     = Complex 0 2
            , expectedA    = Complex (-2) 0
            }
    , CaseA { descriptionA = "Multiply numbers with real and imaginary part"
            , number1A     = Complex 1 2
            , number2A     = Complex 3 4
            , expectedA    = Complex (-5) 10
            }
    ]

divCases :: [CaseA]
divCases =
    [ CaseA { descriptionA = "Divide purely real numbers"
            , number1A     = Complex 1 0
            , number2A     = Complex 2 0
            , expectedA    = Complex 0.5 0
            }
    , CaseA { descriptionA = "Divide purely imaginary numbers"
            , number1A     = Complex 0 1
            , number2A     = Complex 0 2
            , expectedA    = Complex 0.5 0
            }
    , CaseA { descriptionA = "Divide numbers with real and imaginary part"
            , number1A     = Complex 1 2
            , number2A     = Complex 3 4
            , expectedA    = Complex 0.44 0.08
            }
    ]

addCases :: [CaseA]
addCases =
    [ CaseA { descriptionA = "Add purely real numbers"
            , number1A     = Complex 1 0
            , number2A     = Complex 2 0
            , expectedA    = Complex 3 0
            }
    , CaseA { descriptionA = "Add purely imaginary numbers"
            , number1A     = Complex 0 1
            , number2A     = Complex 0 2
            , expectedA    = Complex 0 3
            }
    , CaseA { descriptionA = "Add numbers with real and imaginary part"
            , number1A     = Complex 1 2
            , number2A     = Complex 3 4
            , expectedA    = Complex 4 6
            }
    ]

subCases :: [CaseA]
subCases =
    [ CaseA { descriptionA = "Subtract purely real numbers"
            , number1A     = Complex 1 0
            , number2A     = Complex 2 0
            , expectedA    = Complex (-1) 0
            }
    , CaseA { descriptionA = "Subtract purely imaginary numbers"
            , number1A     = Complex 0 1
            , number2A     = Complex 0 2
            , expectedA    = Complex 0 (-1)
            }
    , CaseA { descriptionA = "Subtract numbers with real and imaginary part"
            , number1A     = Complex 1 2
            , number2A     = Complex 3 4
            , expectedA    = Complex (-2) (-2)
            }
    ]
