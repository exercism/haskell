{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Squares (difference, squareOfSums, sumOfSquares)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "differenceOfSquares" $ do

    -- Test cases adapted from `exercism/x-common/difference-of-squares.json`
    -- on 2016-08-03.

    describe "squareOfSums" $ do
      it "square of sum 5"   $ squareOfSums   5 `shouldBe`      225
      it "square of sum 10"  $ squareOfSums  10 `shouldBe`     3025
      it "square of sum 100" $ squareOfSums 100 `shouldBe` 25502500

    describe "sumOfSquares" $ do
      it "sum of squares 5"   $ sumOfSquares   5 `shouldBe`     55
      it "sum of squares 10"  $ sumOfSquares  10 `shouldBe`    385
      it "sum of squares 100" $ sumOfSquares 100 `shouldBe` 338350

    describe "differenceOfSquares" $ do
      it "difference of squares 0"   $ difference   0 `shouldBe`        0
      it "difference of squares 5"   $ difference   5 `shouldBe`      170
      it "difference of squares 10"  $ difference  10 `shouldBe`     2640
      it "difference of squares 100" $ difference 100 `shouldBe` 25164150

    -- Track-specific tests.

    describe "Integral tests" $ do

      describe "squareOfSums" $ do

        it "squareOfSums (6 :: Int)" $
          squareOfSums (6 :: Int)
          `shouldBe` (441 :: Int)

        it "squareOfSums (7 :: Integer)" $
          squareOfSums (7 :: Integer)
          `shouldBe` (784 :: Integer)

      describe "sumOfSquares" $ do

        it "sumOfSquares (8 :: Int)" $
          sumOfSquares (8 :: Int)
          `shouldBe` (204 :: Int)

        it "sumOfSquares (9 :: Integer)" $
          sumOfSquares (9 :: Integer)
          `shouldBe` (285 :: Integer)

      describe "difference" $ do

        it "difference (11 :: Int)" $
          difference (11 :: Int)
          `shouldBe` (3850 :: Int)

        it "difference (12 :: Integer)" $
          difference (12 :: Integer)
          `shouldBe` (5434 :: Integer)

      {-
      describe "huge difference" $
        it "difference (1234567890 :: Integer)" $
          difference (1234567890 :: Integer)
          `shouldBe` (580764307309260838625720836817589660 :: Integer)
      -}
