import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import Data.List (sort)
import CustomSet (CustomSet)
import qualified CustomSet as S

-- If you're interested in writing an efficient implementation but don't quite
-- know where to start, the best primer I know of is Chris Okasaki's
-- "Purely Functional Data Structures", which you can read a version of here:
-- https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList customSetTests ]

intSet :: [Int] -> CustomSet Int
intSet = S.fromList

customSetTests :: [Test]
customSetTests =
    -- exercism/x-common/custom-set.json cases.
  [ testCase "empty" $ do
    True @=? S.null (intSet [])
    False @=? S.null (intSet [1])
  , testCase "contains" $ do
    False @=? 1 `S.member` intSet []
    True @=? 1 `S.member` intSet [1..3]
    False @=? 4 `S.member` intSet [1..3]
  , testCase "subset" $ do
    True @=? intSet [] `S.isSubsetOf` intSet []
    True @=? intSet [] `S.isSubsetOf` intSet [1]
    False @=? intSet [1] `S.isSubsetOf` intSet []
    True @=? intSet [1..3] `S.isSubsetOf` intSet [1..3]
    True @=? intSet [1..3] `S.isSubsetOf` intSet [4, 1, 2, 3]
    False @=? intSet [1..3] `S.isSubsetOf` intSet [4, 1, 3]
  , testCase "disjoint" $ do
    True @=? intSet [] `S.isDisjointFrom` intSet []
    True @=? intSet [] `S.isDisjointFrom` intSet [1]
    True @=? intSet [1] `S.isDisjointFrom` intSet []
    False @=? intSet [1, 2] `S.isDisjointFrom` intSet [2, 3]
    True @=? intSet [1, 2] `S.isDisjointFrom` intSet [3, 4]
  , testCase "equal" $ do
    True @=? intSet [] == intSet []
    False @=? intSet [] == intSet [1..3]
    False @=? intSet [1..3] == intSet []
    True @=? intSet [1, 2] == intSet [2, 1]
    False @=? intSet [1, 2, 3] == intSet [1, 2, 4]
  , testCase "add" $ do
    intSet [3] @=? 3 `S.insert` S.fromList []
    intSet [1..4] @=? 3 `S.insert` S.fromList [1, 2, 4]
    intSet [1..3] @=? 3 `S.insert` S.fromList [1, 2, 3]
  , testCase "intersection" $ do
    intSet [] @=? intSet [] `S.intersection` intSet []
    intSet [] @=? intSet [] `S.intersection` intSet [3, 2, 5]
    intSet [] @=? intSet [1..4] `S.intersection` intSet []
    intSet [] @=? intSet [1..3] `S.intersection` intSet [4..6]
    intSet [2, 3] @=? intSet [1..4] `S.intersection` intSet [3, 2, 5]
  , testCase "difference" $ do
    intSet [] @=? intSet [] `S.difference` intSet []
    intSet [] @=? intSet [] `S.difference` intSet [3, 2, 5]
    intSet [1..4] @=? intSet [1..4] `S.difference` intSet []
    intSet [1, 3] @=? intSet [3, 2, 1] `S.difference` intSet [2, 4]
  , testCase "union" $ do
    intSet [] @=? intSet [] `S.union` intSet []
    intSet [2] @=? intSet [] `S.union` intSet [2]
    intSet [1, 3] @=? intSet [1, 3] `S.union` intSet []
    intSet [3, 2, 1] @=? intSet [1, 3] `S.union` intSet [2, 3]
    -- additional cases.
  , testCase "Show" $ do
    "fromList []" @=? show (S.empty :: CustomSet Int)
    "fromList [1,2,3]" @=? show (intSet [1, 2, 3])
  , testCase "delete" $ do
    intSet [1, 3] @=? S.delete 2 (intSet [1, 2, 3])
  , testCase "size" $ do
    0 @=? S.size (intSet [])
    3 @=? S.size (intSet [1, 2, 3])
    3 @=? S.size (intSet [1, 2, 3, 2])
    3 @=? S.size (3 `S.insert` intSet [1, 2])
    3 @=? S.size (3 `S.delete` intSet [1..4])
  , testCase "toList" $ do
    [] @=? sort (S.toList (intSet []))
    [1, 2, 3] @=? sort (S.toList (intSet [3, 1, 2]))
    [1, 2, 3] @=? sort (S.toList (intSet [3, 1, 2, 1]))
  ]
