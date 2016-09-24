{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import Forth (ForthError(..), evalText, empty, formatStack)
import Control.Monad (foldM)
import Data.Text (Text)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList forthTests ]

runTexts :: [Text] -> Either ForthError Text
runTexts xs = formatStack <$> foldM (flip evalText) empty xs

forthTests :: [Test]
forthTests =
  [ testCase "no input, no stack" $
    "" @=? formatStack empty
  , testCase "numbers just get pushed onto the stack" $
    Right "1 2 3 4 5" @=? runTexts ["1 2 3 4 5"]
  , testCase "non-word characters are separators" $
    -- Note the Ogham Space Mark ( ), this is a spacing character.
    Right "1 2 3 4 5 6 7" @=? runTexts ["1\NUL2\SOH3\n4\r5 6\t7"]
  , testCase "basic arithmetic" $ do
    Right "-1" @=? runTexts ["1 2 + 4 -"]
    Right "2" @=? runTexts ["2 4 * 3 /"]
  , testCase "division by zero" $
    Left DivisionByZero @=? runTexts ["4 2 2 - /"]
  , testCase "dup" $ do
    Right "1 1" @=? runTexts ["1 DUP"]
    Right "1 2 2" @=? runTexts ["1 2 Dup"]
    Left StackUnderflow @=? runTexts ["dup"]
  , testCase "drop" $ do
    Right "" @=? runTexts ["1 drop"]
    Right "1" @=? runTexts ["1 2 drop"]
    Left StackUnderflow @=? runTexts ["drop"]
  , testCase "swap" $ do
    Right "2 1" @=? runTexts ["1 2 swap"]
    Right "1 3 2" @=? runTexts ["1 2 3 swap"]
    Left StackUnderflow @=? runTexts ["1 swap"]
    Left StackUnderflow @=? runTexts ["swap"]
  , testCase "over" $ do
    Right "1 2 1" @=? runTexts ["1 2 over"]
    Right "1 2 3 2" @=? runTexts ["1 2 3 over"]
    Left StackUnderflow @=? runTexts ["1 over"]
    Left StackUnderflow @=? runTexts ["over"]
  , testCase "defining a new word" $
    Right "1 1 1" @=? runTexts [ ": dup-twice dup dup ;"
                               , "1 dup-twice"
                               ]
  , testCase "redefining an existing word" $
    Right "1 1 1" @=? runTexts [ ": foo dup ;"
                               , ": foo dup dup ;"
                               , "1 foo"
                               ]
  , testCase "redefining an existing built-in word" $
    Right "1 1" @=? runTexts [ ": swap dup ;"
                             , "1 swap"
                             ]
  , testCase "defining words with odd characters" $
    Right "220371" @=? runTexts [": € 220371 ; €"]
  , testCase "defining a number" $
    Left InvalidWord @=? runTexts [": 1 2 ;"]
  , testCase "calling a non-existing word" $
    Left (UnknownWord "foo") @=? runTexts ["1 foo"]
  ]
