{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumDecimals #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Data.String       (fromString)

import ResistorColors (Color(..), Resistor(..), label, ohms)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
  describe "label" $ for_ cases testLabel
  describe "ohms" $ for_ cases testOhms
  where
    testLabel Case{..} = it (description input) $
      label input `shouldBe` fromString expectedLabel
      where
        description Resistor{..} =
          show bands ++ " should give " ++ expectedLabel

    testOhms Case{..} = it (description input) $
      ohms input `shouldBe` expectedOhms
      where
        description Resistor{..} =
          show bands ++ " should give " ++ show expectedOhms

data Case = Case { input         :: Resistor
                 , expectedLabel :: String
                 , expectedOhms  :: Int
                 }

cases :: [Case]
cases = [ Case { input         = Resistor (Black, Black, Black)
               , expectedLabel = "0 ohms"
               , expectedOhms  = 0
               }

        , Case { input         = Resistor (Black, Yellow, Black)
               , expectedLabel = "4 ohms"
               , expectedOhms  = 4
               }
        , Case { input         = Resistor (Grey, Orange, Black)
               , expectedLabel = "83 ohms"
               , expectedOhms  = 83
               }
        , Case { input         = Resistor (Blue, Grey, Brown)
               , expectedLabel = "680 ohms"
               , expectedOhms  = 680
               }
        , Case { input         = Resistor (Red, Black, Red)
               , expectedLabel = "2 kiloohms"
               , expectedOhms  = 2e3
               }
        , Case { input         = Resistor (Green, Brown, Orange)
               , expectedLabel = "51 kiloohms"
               , expectedOhms  = 51e3
               }
        , Case { input         = Resistor (White, Violet, Yellow)
               , expectedLabel = "970 kiloohms"
               , expectedOhms  = 970e3
               }

        , Case { input         = Resistor (White, Black, Green)
               , expectedLabel = "9 megaohms"
               , expectedOhms  = 9e6
               }
        , Case { input         = Resistor (Brown, Grey, Blue)
               , expectedLabel = "18 megaohms"
               , expectedOhms  = 18e6
               }
        , Case { input         = Resistor (Red, Violet, Violet)
               , expectedLabel = "270 megaohms"
               , expectedOhms  = 270e6
               }
        , Case { input         = Resistor (Yellow, Black, Grey)
               , expectedLabel = "4 gigaohms"
               , expectedOhms  = 4e9
               }
        , Case { input         = Resistor (Orange, Blue, White)
               , expectedLabel = "36 gigaohms"
               , expectedOhms  = 36e9
               }
        {-
           -- The following tests are commented out to decrease the
           -- complexity of the exercise. Students may choose freely
           -- to include them or not.
        , Case { input         = Resistor (Brown, Red, Red)
               , expectedLabel = "1.2 kiloohms"
               , expectedOhms  = 1.2e3
               }
        , Case { input         = Resistor (Orange, Yellow, Green)
               , expectedLabel = "3.4 megaohms"
               , expectedOhms  = 3.4e6
               }
        , Case { input         = Resistor (Green, Blue, Grey)
               , expectedLabel = "5.6 gigaohms"
               , expectedOhms  = 5.6e9
               }
        -}
        ]
