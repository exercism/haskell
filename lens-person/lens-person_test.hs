import           Data.Char          (toUpper)
import           Data.Time.Calendar (fromGregorian)
import           System.Exit        (ExitCode (..), exitWith)
import           Test.HUnit         (Assertion, Counts (..), Test (..),
                                     runTestTT, (@=?))

import           Person             (Address (..), Born (..),
                                     Name (..), Person (..), bornStreet,
                                     renameStreets, setBirthMonth,
                                     setCurrentStreet)

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList personTests ]

testPerson :: Person
testPerson = Person {
               _name = Name {
                   _foreNames = "Jane Joanna",
                   _surName = "Doe"
               },
               _born = Born {
                         _bornAt = Address {
                             _street = "Longway",
                             _houseNumber = 1024,
                             _place = "Springfield",
                             _country = "United States"
                         },
                         _bornOn = fromGregorian 1984 4 12
                       },
               _address = Address {
                            _street = "Shortlane",
                            _houseNumber = 2,
                            _place = "Fallmeadow",
                            _country = "Canada"
                          }
             }

personTests :: [Test]
personTests =
  [ testCase "bornStreet" $
    "Longway" @=? bornStreet (_born testPerson),
    testCase "setCurrentStreet" $
    "Middleroad" @=? (_street . _address) (setCurrentStreet "Middleroad" testPerson),
    testCase "setBirthMonth" $
    fromGregorian 1984 9 12 @=? (_bornOn . _born) (setBirthMonth 9 testPerson),
    testCase "renameStreets birth" $
    "LONGWAY" @=? (_street . _bornAt . _born) (renameStreets (map toUpper) testPerson),
    testCase "renameStreets current" $
    "SHORTLANE" @=? (_street . _address) (renameStreets (map toUpper) testPerson)
  ]
