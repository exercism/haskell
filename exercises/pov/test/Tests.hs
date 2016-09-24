import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import Data.List (sort)
import System.Exit (ExitCode(..), exitWith)
import POV(Graph(..), fromPOV, tracePathBetween)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList ((tracePathTest : untraceableTest : leafToLeaf : reparentingTests) ++ notFoundTests) ]

leaf :: a -> Graph a
leaf v = Graph v []

x :: String
x = "x"

root :: Graph a -> a
root (Graph r _) = r

-- In the trees we're making, we don't care about the ordering of children.
-- This is significant when rerooting on nodes that have a parent and children.
-- The former parent can go either before or after the former children.
-- Either choice would be correct in the context of this problem.
-- So all we need to check is:
-- 1) The graph is actually rooted on the requested node.
-- 2) The sorted edge list is correct.
-- This function helps check the second condition.
edges :: Graph a -> [(a, a)]
edges (Graph r children) = map ((,) r . root) children ++ concatMap edges children

singleton, flat, kids, nested, cousins :: Graph String
singleton = Graph x []
flat = Graph "root" (map leaf ["a", "b", x, "c"])
nested = Graph "level-0" [Graph "level-1" [Graph "level-2" [Graph "level-3" [Graph x []]]]]
kids = Graph "root" [Graph x [Graph "kid-0" [], Graph "kid-1" []]]
cousins = Graph "grandparent" [Graph "parent" [Graph x [leaf "kid-a", leaf "kid-b"],
                                               leaf "sibling-0",
                                               leaf "sibling-1"],
                                Graph "uncle" [leaf "cousin-0",
                                               leaf "cousin-1"]]

singleton', flat', nested', kids', cousins' :: Graph String
singleton' = singleton
flat' = Graph x [Graph "root" (map leaf ["a", "b", "c"])]
nested' = Graph x [Graph "level-3" [Graph "level-2" [Graph "level-1" [Graph "level-0" []]]]]
kids' = Graph x [Graph "kid-0" [], Graph "kid-1" [], Graph "root" []]
cousins' = Graph x [leaf "kid-a",
                    leaf "kid-b",
                    Graph "parent" [Graph "sibling-0" [],
                                   Graph "sibling-1" [],
                                   Graph "grandparent" [
                                                        Graph "uncle" [Graph "cousin-0" [],
                                                                       Graph "cousin-1" []]]]]

reparentTestCases :: [(String, Graph String, Maybe (Graph String))]
reparentTestCases = [
    ("reparenting singleton", singleton, Just singleton'),
    ("reparenting flat", flat, Just flat'),
    ("reparenting nested", nested, Just nested'),
    ("reparenting kids", kids, Just kids'),
    ("reparenting cousins", cousins, Just cousins'),
    ("from POV of non-existent node", leaf "foo", Nothing)]

reparentingTests :: [Test]
reparentingTests = do
    (name, input, output) <- reparentTestCases
    [checkRoot name input output, checkEdges name input output]
      where graphTestCase f testName name input output =
              testCase (name ++ ": correct " ++ testName) $ f output @=? f (fromPOV x input)
            checkRoot = graphTestCase (fmap root) "root"
            checkEdges = graphTestCase sortedEdges "sorted edges"
            sortedEdges = fmap (sort . edges)

notFoundTests :: [Test]
notFoundTests = map notFoundTest [singleton, flat, kids, nested, cousins]
    where name = "Should not be able to find a missing node"
          notFoundTest g = testCase name $ Nothing @=? fromPOV "NOT THERE" g

untraceableTest :: Test
untraceableTest = testCase "Cannot trace between un-connected nodes" assertion
    where assertion = Nothing @=? tracePathBetween x "NOT THERE" cousins

tracePathTest :: Test
tracePathTest = testCase "Can trace a path from x -> cousin" assertion
    where assertion = expectedPath @=? tracePathBetween x "cousin-1" cousins
          expectedPath = Just ["x", "parent", "grandparent", "uncle", "cousin-1"]

leafToLeaf :: Test
leafToLeaf = testCase "Can trace from a leaf to a leaf" assertion
    where assertion = expectedPath @=? tracePathBetween "kid-a" "cousin-0" cousins
          expectedPath = Just ["kid-a", "x", "parent", "grandparent", "uncle", "cousin-0"]
