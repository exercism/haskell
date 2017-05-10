{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Grains (square, total)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
          describe "square" $ for_ squareCases squareTest
          describe "total"  $ totalTest totalCase
  where

    squareTest (description, n, expected) = it description assertion
      where
        assertion  = expression `shouldBe` expected
        expression = fmap fromIntegral . square . fromIntegral $ n

    totalTest (description, expected) = it description assertion
      where
        assertion = fromIntegral total `shouldBe` expected

squareCases :: [(String, Integer, Maybe Integer)]
squareCases =
    [ ("square 1"             ,  1, Just                   1)
    , ("square 2"             ,  2, Just                   2)
    , ("square 3"             ,  3, Just                   4)
    , ("square 4"             ,  4, Just                   8)
    , ("square 16"            , 16, Just               32768)
    , ("square 32"            , 32, Just          2147483648)
    , ("square 64"            , 64, Just 9223372036854775808)
    , ("square negative"      , -1, Nothing                 )
    , ("square 0"             ,  0, Nothing                 )
    , ("square bigger than 64", 65, Nothing                 ) ]

totalCase :: (String, Integer)
totalCase = ("total grains", 18446744073709551615)
