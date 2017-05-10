{-# LANGUAGE OverloadedStrings #-}

import Control.Monad     (foldM)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Forth (ForthError(..), empty, evalText, toList)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    let runTexts = fmap toList . foldM (flip evalText) empty

    describe "parsing and numbers" $ do
      it "empty input results in empty stack" $
        toList empty `shouldBe` []

      it "numbers just get pushed onto the stack" $
        runTexts ["1 2 3 4 5"] `shouldBe` Right [1, 2, 3, 4, 5]

      it "all non-word characters are separators" $
        runTexts ["1\NUL2\SOH3\n4\r5 6\t7"] `shouldBe` Right [1, 2, 3, 4, 5, 6, 7]

    describe "addition" $ do
      it "can add two numbers" $
        runTexts ["1 2 +"] `shouldBe` Right [3]
      it "errors if there is nothing on the stack" $
        runTexts ["+"] `shouldBe` Left StackUnderflow
      it "errors if there is only one value on the stack" $
        runTexts ["1 +"] `shouldBe` Left StackUnderflow

    describe "subtraction" $ do
      it "can subtract two numbers" $
        runTexts ["3 4 -"] `shouldBe` Right [-1]
      it "errors if there is nothing on the stack" $
        runTexts ["-"] `shouldBe` Left StackUnderflow
      it "errors if there is only one value on the stack" $
        runTexts ["1 -"] `shouldBe` Left StackUnderflow

    describe "multiplication" $ do
      it "can multiply two numbers" $
        runTexts ["2 4 *"] `shouldBe` Right [8]
      it "errors if there is nothing on the stack" $
        runTexts ["*"] `shouldBe` Left StackUnderflow
      it "errors if there is only one value on the stack" $
        runTexts ["1 *"] `shouldBe` Left StackUnderflow

    describe "division" $ do
      it "can divide two numbers" $
        runTexts ["12 3 /"] `shouldBe` Right [4]
      it "performs integer division" $
        runTexts ["8 3 /"] `shouldBe` Right [2]
      it "errors if dividing by zero" $
        runTexts ["4 0 /"] `shouldBe` Left DivisionByZero
      it "errors if there is nothing on the stack" $
        runTexts ["/"] `shouldBe` Left StackUnderflow
      it "errors if there is only one value on the stack" $
        runTexts ["1 /"] `shouldBe` Left StackUnderflow

    describe "combined arithmetic" $ do
      it "addition and subtraction" $
        runTexts ["1 2 + 4 -"] `shouldBe` Right [-1]

      it "multiplication and division" $
        runTexts ["2 4 * 3 /"] `shouldBe` Right [2]

    describe "dup" $ do
      it "copies the top value on the stack" $
        runTexts ["1 DUP"  ] `shouldBe` Right [1, 1]
      it "is case-insensitive" $
        runTexts ["1 2 Dup"] `shouldBe` Right [1, 2, 2]
      it "errors if there is nothing on the stack" $
        runTexts ["dup"    ] `shouldBe` Left StackUnderflow

    describe "drop" $ do
      it "removes the top value on the stack if it is the only one" $
        runTexts ["1 drop"  ] `shouldBe` Right []
      it "removes the top value on the stack if it is not the only one" $
        runTexts ["1 2 drop"] `shouldBe` Right [1]
      it "errors if there is nothing on the stack" $
        runTexts ["drop"    ] `shouldBe` Left StackUnderflow

    describe "swap" $ do
      it "swaps the top two values on the stack if they are the only ones" $
        runTexts ["1 2 swap"  ] `shouldBe` Right [2, 1]
      it "swaps the top two values on the stack if they are not the only ones" $
        runTexts ["1 2 3 swap"] `shouldBe` Right [1, 3, 2]
      it "errors if there is nothing on the stack" $
        runTexts ["swap"      ] `shouldBe` Left StackUnderflow
      it "errors if there is only one value on the stack" $
        runTexts ["1 swap"    ] `shouldBe` Left StackUnderflow

    describe "over" $ do
      it "copies the second element if there are only two" $
        runTexts ["1 2 over"  ] `shouldBe` Right [1, 2, 1]
      it "copies the second element if there are more than two" $
        runTexts ["1 2 3 over"] `shouldBe` Right [1, 2, 3, 2]
      it "errors if there is nothing on the stack" $
        runTexts ["over"      ] `shouldBe` Left StackUnderflow
      it "errors if there is only one value on the stack" $
        runTexts ["1 over"    ] `shouldBe` Left StackUnderflow

    describe "user-defined words" $ do
      it "can consist of built-in words" $
        runTexts [ ": dup-twice dup dup ;"
                 , "1 dup-twice"           ] `shouldBe` Right [1, 1, 1]

      it "execute in the right order" $
        runTexts [ ": countup 1 2 3 ;"
                 , "countup"           ] `shouldBe` Right [1, 2, 3]

      it "can override other user-defined words" $
        runTexts [ ": foo dup ;"
                 , ": foo dup dup ;"
                 , "1 foo"           ] `shouldBe` Right [1, 1, 1]

      it "can override built-in words" $
        runTexts [ ": swap dup ;"
                 , "1 swap"       ] `shouldBe` Right [1, 1]

      it "cannot redefine numbers" $
        runTexts [": 1 2 ;"] `shouldBe` Left InvalidWord

      it "errors if executing a non-existent word" $
        runTexts ["1 foo"] `shouldBe` Left (UnknownWord "foo")
