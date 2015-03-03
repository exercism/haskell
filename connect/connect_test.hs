import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import Connect (resultFor, Color(Black, White))
import Prelude hiding (lines) -- don't complain about redefining lines

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

-- | Remove spaces to turn a readable board into valid input for resultFor.
makeBoard :: [String] -> [String]
makeBoard = map (filter (/=' '))

test_emptyBoard :: Assertion
test_emptyBoard =
  let lines = [". . . . ."
              ," . . . . ."
              ,"  . . . . ."
              ,"   . . . . ."
              ,"    . . . . ."
              ]
  in Nothing @=? resultFor (makeBoard lines)

test_oneByOneBlack :: Assertion
test_oneByOneBlack =
  let lines = ["X"]
  in Just Black @=? resultFor (makeBoard lines)

test_oneByOneWhite :: Assertion
test_oneByOneWhite =
  let lines = ["O"]
  in Just White @=? resultFor (makeBoard lines)

test_convultedPath :: Assertion
test_convultedPath =
  let lines = [". X X . ."
              ," X . X . X"
              ,"  . X . X ."
              ,"   . X X . ."
              ,"    O O O O O"
              ]
  in Just Black @=? resultFor (makeBoard lines)

test_rectangleBlack :: Assertion
test_rectangleBlack =
  let lines = [". O . ."
              ," O X X X"
              ,"  O X O ."
              ,"   X X O X"
              ,"    . O X ."
              ]
  in Just Black @=? resultFor (makeBoard lines)

test_rectangleWhite :: Assertion
test_rectangleWhite =
  let lines = [". O . ."
              ," O X X X"
              ,"  O O O ."
              ,"   X X O X"
              ,"    . O X ."
              ]
  in Just White @=? resultFor (makeBoard lines)

test_spiralBlack :: Assertion
test_spiralBlack =
  let board = ["OXXXXXXXX"
              ,"OXOOOOOOO"
              ,"OXOXXXXXO"
              ,"OXOXOOOXO"
              ,"OXOXXXOXO"
              ,"OXOOOXOXO"
              ,"OXXXXXOXO"
              ,"OOOOOOOXO"
              ,"XXXXXXXXO"
              ]
  in Just Black @=? resultFor board

test_spiralNone :: Assertion
test_spiralNone =
  let board = ["OXXXXXXXX"
              ,"OXOOOOOOO"
              ,"OXOXXXXXO"
              ,"OXOXOOOXO"
              ,"OXOX.XOXO"
              ,"OXOOOXOXO"
              ,"OXXXXXOXO"
              ,"OOOOOOOXO"
              ,"XXXXXXXXO"
              ]
  in Nothing @=? resultFor board

resultForTests :: [Test]
resultForTests =
  [ testCase "empty board has no winner" test_emptyBoard
  , testCase "1x1 board with black stone" test_oneByOneBlack
  , testCase "1x1 board with white stone" test_oneByOneWhite
  , testCase "convulted path" test_convultedPath
  , testCase "rectangle, black wins" test_rectangleBlack
  , testCase "rectangle, white wins" test_rectangleWhite
  , testCase "spiral, black wins" test_spiralBlack
  , testCase "spiral, nobody wins" test_spiralNone
  ]

main :: IO ()
main = exitProperly (runTestTT (TestList resultForTests))
