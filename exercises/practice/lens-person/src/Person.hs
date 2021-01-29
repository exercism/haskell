module Person
  ( Address (..)
  , Born    (..)
  , Name    (..)
  , Person  (..)
  , bornStreet
  , renameStreets
  , setBirthMonth
  , setCurrentStreet
  ) where

import Data.Time.Calendar (Day)

data Person = Person { _name    :: Name
                     , _born    :: Born
                     , _address :: Address
                     }

data Name = Name { _foreNames :: String
                 , _surName   :: String
                 }

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 }

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       }

bornStreet :: Born -> String
bornStreet born = error "You need to implement this function."

setCurrentStreet :: String -> Person -> Person
setCurrentStreet street person = error "You need to implement this function."

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person = error "You need to implement this function."

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = error "You need to implement this function."
