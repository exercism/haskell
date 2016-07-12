import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import Queens (boardString, canAttack)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
  [ TestList queenTests ]

emptyBoard, board, boardWithJustW, boardWithJustB :: String
emptyBoard = concat [ "_ _ _ _ _ _ _ _\n"
                    , "_ _ _ _ _ _ _ _\n"
                    , "_ _ _ _ _ _ _ _\n"
                    , "_ _ _ _ _ _ _ _\n"
                    , "_ _ _ _ _ _ _ _\n"
                    , "_ _ _ _ _ _ _ _\n"
                    , "_ _ _ _ _ _ _ _\n"
                    , "_ _ _ _ _ _ _ _\n"
                    ]
board = concat [ "_ _ _ _ _ _ _ _\n"
               , "_ _ _ _ _ _ _ _\n"
               , "_ _ _ _ W _ _ _\n"
               , "_ _ _ _ _ _ _ _\n"
               , "_ _ _ _ _ _ _ _\n"
               , "_ _ _ _ _ _ _ _\n"
               , "_ _ _ _ _ _ B _\n"
               , "_ _ _ _ _ _ _ _\n"
               ]
boardWithJustW = concat [ "_ _ _ _ _ _ _ _\n"
                        , "_ _ _ _ _ _ _ _\n"
                        , "_ _ _ _ W _ _ _\n"
                        , "_ _ _ _ _ _ _ _\n"
                        , "_ _ _ _ _ _ _ _\n"
                        , "_ _ _ _ _ _ _ _\n"
                        , "_ _ _ _ _ _ _ _\n"
                        , "_ _ _ _ _ _ _ _\n"
                        ]
boardWithJustB = concat [ "B _ _ _ _ _ _ _\n"
                        , "_ _ _ _ _ _ _ _\n"
                        , "_ _ _ _ _ _ _ _\n"
                        , "_ _ _ _ _ _ _ _\n"
                        , "_ _ _ _ _ _ _ _\n"
                        , "_ _ _ _ _ _ _ _\n"
                        , "_ _ _ _ _ _ _ _\n"
                        , "_ _ _ _ _ _ _ _\n"
                        ]               

queenTests :: [Test]
queenTests =
  [ testCase "empty board" $ emptyBoard @=? boardString Nothing Nothing
  , testCase "board with just white queen" $ boardWithJustW @=? boardString (Just (2, 4)) Nothing
  , testCase "board with just black queen" $ boardWithJustB @=? boardString Nothing (Just (0, 0))
  , testCase "board" $ board @=? boardString (Just (2, 4)) (Just (6, 6))
  , TestLabel "attacks" $ TestList attackTests
  ]

attackTests :: [Test]
attackTests = [testCase (show a ++ " should" ++ (if expected then "" else " not") ++ " threaten " ++ show b)
                        (expected @=? canAttack a b) | (expected, a, b) <- attackCases]

attackCases :: [(Bool, (Int, Int), (Int, Int))]
attackCases = [
    (False, (2, 3), (4, 7)),
    (True, (2, 4), (2, 7)),
    (True, (5, 4), (2, 4)),
    (True, (1, 1), (6, 6)),
    (True, (0, 6), (1, 7)),
    (True, (4, 1), (6, 3)),
    (True, (2, 2), (1, 3))]

