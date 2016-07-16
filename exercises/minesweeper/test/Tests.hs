import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import Minesweeper (annotate)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList minesweeperTests ]

clean :: [String] -> [String]
clean = map (map mineOrSpace)
  where mineOrSpace '*' = '*'
        mineOrSpace _   = ' '

check :: [String] -> Assertion
check board = board @=? annotate (clean board)

minesweeperTests :: [Test]
minesweeperTests =
  [ testCase "zero size board" $ do
    check []
  , testCase "empty board" $ do
    check [ "   "
          , "   "
          , "   "
          ]
  , testCase "board full of mines" $ do
    check [ "***"
          , "***"
          , "***"
          ]
  , testCase "surrounded" $ do
    check [ "***"
          , "*8*"
          , "***"
          ]
  , testCase "horizontal line" $ do
    check [ "1*2*1" ]
  , testCase "vertical line" $ do
    check [ "1"
          , "*"
          , "2"
          , "*"
          , "1"
          ]
  , testCase "cross" $ do
    check [ " 2*2 "
          , "25*52"
          , "*****"
          , "25*52"
          , " 2*2 "
          ]
  ]
