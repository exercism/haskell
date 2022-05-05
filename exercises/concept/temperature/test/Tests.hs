{-# LANGUAGE RecordWildCards #-}

import Data.Foldable (for_)
import Test.Hspec    (describe, hspec, it, shouldBe)

import Temperature (tempToC, tempToF)

data Case = Case { description :: String
                 , fahrenheit :: Integer
                 , celsius :: Float
                 }

toFCases :: [Case]
toFCases = [ Case { description = "negative Celsius to negative Fahrenheit"
                  , celsius     = -100.0
                  , fahrenheit  = -148
                  }
           , Case { description = "negative Celsius to zero Fahrenheit"
                  , celsius     = -18.0
                  , fahrenheit  = 0
                  }
           , Case { description = "zero Celsius to positive Fahrenheit"
                  , celsius     = 0.0
                  , fahrenheit  = 32
                  }
           , Case { description = "positive Celsius to positive Fahrenheit"
                  , celsius     = 50.10
                  , fahrenheit  = 123
                  }
           ]

toCCases :: [Case]
toCCases = [ Case { description = "negative Fahrenheit to negative Celsius"
                  , fahrenheit  = -148
                  , celsius     = -100.0
                  }
           , Case { description = "zero Fahrenheit to negative Celsius"
                  , fahrenheit  = 0
                  , celsius     = -17.78
                  }
           , Case { description = "positive Fahrenheit to zero Celsius"
                  , fahrenheit  = 32
                  , celsius     = 0.0
                  }
           , Case { description = "positive Fahrenheit to positive Celsius"
                  , fahrenheit  = 123
                  , celsius     = 50.56
                  }
           ]

main :: IO ()
main = hspec $ do
       describe "tempToF" $ do
         for_ toFCases $ \Case {..} ->
           it description $ tempToF celsius `shouldBe` fahrenheit
       describe "tempToC" $ do
         for_ toCCases $ \Case {..} ->
           it description $ roundTo 2 (tempToC fahrenheit) `shouldBe` celsius

roundTo :: Int -> Float -> Float
roundTo n = (/ 10 ^ n) . (fromIntegral :: Integer -> Float) . round . (* 10 ^ n)
