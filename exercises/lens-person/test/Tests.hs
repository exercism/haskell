import Data.Char          (toUpper)
import Data.Time.Calendar (fromGregorian)
import Test.Hspec         (Spec, it, shouldBe)
import Test.Hspec.Runner  (configFastFail, defaultConfig, hspecWith)

import Person
  ( Address (..)
  , Born    (..)
  , Name    (..)
  , Person  (..)
  , bornStreet
  , renameStreets
  , setBirthMonth
  , setCurrentStreet
  )

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    it "bornStreet" $
      (bornStreet . _born) testPerson
      `shouldBe` "Longway"

    it "setCurrentStreet" $
      (_street . _address . setCurrentStreet "Middleroad") testPerson
      `shouldBe` "Middleroad"

    it "setBirthMonth" $
      (_bornOn . _born . setBirthMonth 9) testPerson
      `shouldBe` fromGregorian 1984 9 12

    it "renameStreets birth" $
      (_street . _bornAt . _born . renameStreets (map toUpper)) testPerson
      `shouldBe` "LONGWAY"

    it "renameStreets current" $
      (_street . _address . renameStreets (map toUpper)) testPerson
      `shouldBe` "SHORTLANE"

testPerson :: Person
testPerson = Person {
               _name = Name {
                         _foreNames = "Jane Joanna",
                         _surName   = "Doe"
                       },
               _born = Born {
                         _bornAt = Address {
                                     _street      = "Longway"      ,
                                     _houseNumber = 1024           ,
                                     _place       = "Springfield"  ,
                                     _country     = "United States"
                                   },
                         _bornOn = fromGregorian 1984 4 12
                       },
               _address = Address {
                            _street      = "Shortlane" ,
                            _houseNumber = 2           ,
                            _place       = "Fallmeadow",
                            _country     = "Canada"
                          }
             }
