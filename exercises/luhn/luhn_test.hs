import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import Luhn (checkDigit, addends, checksum, isValid, create)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestLabel "Luhn Tests" $
        TestList luhnTests

int :: Integer -> Integer
int = id

ints :: [Integer] -> [Integer]
ints = id

luhnTests :: [Test]
luhnTests =
  [ testCase "checkDigit" $ do
    int 7 @=? checkDigit 34567
    int 0 @=? checkDigit 91370
  , testCase "addends" $ do
    ints [1, 4, 1, 4, 1] @=? addends 12121
    ints [7, 6, 6, 1] @=? addends 8631
  , TestLabel "checksum" (TestList checksumTests)
  , TestLabel "isValid" (TestList validityTests)
  , TestLabel "create" (TestList creationTests)
  ]

creations :: [(Integer, Integer)]
creations = [(1230, 123)
             ,(8739567, 873956)
             ,(8372637564, 837263756)
             ,(2323200577663554, 232320057766355)]

creationTests :: [Test]
creationTests =
    [testCase ("Creating a valid number from " ++ show n)
              (expected @=? create n) | (expected, n) <- creations]

-- NOTE: this differs from the ruby and js, the checksum really should
--       be mod 10 like we are testing here.
checksums :: [(Integer, Integer)]
checksums = [(4913, 2)
            ,(201773, 1)
            ,(1111, 6)
            ,(8763, 0)
            ,(8739567, 0)
            ,(2323200577663554, 0)]

checksumTests :: [Test]
checksumTests =
    [testCase ("The checksum of " ++ show n)
              (expected @=? checksum n) | (n, expected) <- checksums]

validityChecks :: [(Integer, Bool)]
validityChecks = [(1111, False)
                 ,(738, False)
                 ,(8763, True)
                 ,(8739567, True)
                 ,(2323200577663554, True)]

validityTests :: [Test]
validityTests =
    [testCase (show n ++ " should" ++ (if expected then "" else "n't") ++ " be valid")
              (expected @=? isValid n) | (n, expected) <- validityChecks]
