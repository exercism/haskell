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
  [ testCase "Show" $ do
    "fromList []" @=? show (S.empty :: CustomSet Int)
    "fromList [1,2,3]" @=? show (intSet [1, 2, 3])
  , testCase "delete" $ do
    intSet [1, 3] @=? S.delete 2 (intSet [1, 2, 3])
  , testCase "difference" $ do
    intSet [1, 3] @=? intSet [1, 2, 3] `S.difference` intSet [2, 4]
  , testCase "isDisjointFrom" $ do
    True @=? intSet [] `S.isDisjointFrom` intSet []
    True @=? intSet [1, 2] `S.isDisjointFrom` intSet [3, 4]
    False @=? intSet [1, 2] `S.isDisjointFrom` intSet [2, 3]
  , testCase "null" $ do
    True @=? S.null (intSet [])
    False @=? S.null (intSet [1])
  , testCase "intersection" $ do
    S.fromList "ac" @=? S.fromList "abc" `S.intersection` S.fromList "acd"
    intSet [3] @=? intSet [1, 2, 3] `S.intersection` intSet [3, 4, 5]
  , testCase "member" $ do
    True @=? 2 `S.member` intSet [1..3]
    False @=? 4 `S.member` intSet [1..3]
  , testCase "insert" $ do
    intSet [1..4] @=? 3 `S.insert` S.fromList [1, 2, 4]
    intSet [1..3] @=? 3 `S.insert` S.fromList [1, 2, 3]
  , testCase "size" $ do
    0 @=? S.size (intSet [])
    3 @=? S.size (intSet [1, 2, 3])
    3 @=? S.size (intSet [1, 2, 3, 2])
    3 @=? S.size (3 `S.insert` intSet [1, 2])
    3 @=? S.size (3 `S.delete` intSet [1..4])
  , testCase "isSubsetOf" $ do
    True @=? intSet [1..3] `S.isSubsetOf` intSet [1..3]
    True @=? intSet [1..3] `S.isSubsetOf` intSet [1..4]
    False @=? intSet [1..3] `S.isSubsetOf` intSet [1, 3, 4]
    True @=? intSet [] `S.isSubsetOf` intSet [1, 3, 4]
    True @=? intSet [] `S.isSubsetOf` intSet []
  , testCase "toList" $ do
    [] @=? sort (S.toList (intSet []))
    [1, 2, 3] @=? sort (S.toList (intSet [3, 1, 2]))
    [1, 2, 3] @=? sort (S.toList (intSet [3, 1, 2, 1]))
  , testCase "union" $ do
    intSet [1, 2, 3] @=? intSet [1, 3] `S.union` intSet [2]
    intSet [1, 3] @=? intSet [1, 3] `S.union` intSet []
    intSet [2] @=? intSet [] `S.union` intSet [2]
    intSet [] @=? intSet [] `S.union` intSet []
  ]
