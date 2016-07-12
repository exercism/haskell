{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import ETL (transform)
import qualified Data.Map as M

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList transformTests ]

transformTests :: [Test]
transformTests =
  [ testCase "transform one value" $
    M.fromList [('a', 1)] @=? transform (M.fromList [(1, "A")])
  , testCase "transform multiple keys from one value" $
    M.fromList [('a', 1), ('e', 1)] @=? transform (M.fromList [(1, "AE")])
  , testCase "transform multiple keys from multiple values" $
    M.fromList [('a', 1), ('b', 4)] @=?
    transform (M.fromList [(1, "A"), (4, "B")])
  , testCase "full dataset" $
    M.fromList fullOut @=? transform (M.fromList fullIn)
  ]

fullOut :: [(Char, Int)]
fullOut =
  [ ('a', 1), ('b', 3), ('c', 3), ('d', 2), ('e', 1)
  , ('f', 4), ('g', 2), ('h', 4), ('i', 1), ('j', 8)
  , ('k', 5), ('l', 1), ('m', 3), ('n', 1), ('o', 1)
  , ('p', 3), ('q', 10), ('r', 1), ('s', 1), ('t', 1)
  , ('u', 1), ('v', 4), ('w', 4), ('x', 8), ('y', 4)
  , ('z', 10) ]

fullIn :: [(Int, String)]
fullIn =
  [ (1, "AEIOULNRST")
  , (2, "DG")
  , (3, "BCMP")
  , (4, "FHVWY")
  , (5, "K")
  , (8, "JX")
  , (10,"QZ")
  ]
