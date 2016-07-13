{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import Grains (square, total)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList grainsTests ]

i :: Integral a => a -> Integer
i = fromIntegral

mi :: Integral a => Maybe a -> Maybe Integer
mi = fmap fromIntegral

grainsTests :: [Test]
grainsTests =
  [ testCase "square 1" $
    Just 1 @=? mi (square 1)
  , testCase "square 2" $
    Just 2 @=? mi (square 2)
  , testCase "square 3" $
    Just 4 @=? mi (square 3)
  , testCase "square 4" $
    Just 8 @=? mi (square 4)
  , testCase "square 16" $
    Just 32768 @=? mi (square 16)
  , testCase "square 32" $
    Just 2147483648 @=? mi (square 32)
  , testCase "square 64" $
    Just 9223372036854775808 @=? mi (square 64)
  , testCase "square negative" $
    Nothing @=? mi (square (-1))
  , testCase "square 0" $
    Nothing @=? mi (square 0)
  , testCase "square bigger than 64" $
    Nothing @=? mi (square 65)
  , testCase "total grains" $
    18446744073709551615 @=? i total
  ]
