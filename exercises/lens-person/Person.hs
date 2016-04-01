module Person where

import           Data.Time.Calendar

data Person = Person {
      _name    :: Name,
      _born    :: Born,
      _address :: Address
    }

data Name = Name {
      _foreNames :: String, -- Space separated
      _surName   :: String
    }

data Born = Born {
      _bornAt :: Address,
      _bornOn :: Day
    }

data Address = Address {
      _street      :: String,
      _houseNumber :: Int,
      _place       :: String, -- Village / city
      _country     :: String
    }

-- Valid values of Gregorian are those for which 'Data.Time.Calendar.fromGregorianValid'
-- returns Just.
data Gregorian = Gregorian {
      _year  :: Integer,
      _month :: Int,
      _day   :: Int
    }

-- Implement these.

bornStreet :: Born -> String

setCurrentStreet :: String -> Person -> Person

setBirthMonth :: Int -> Person -> Person

-- | Transform both birth and current street names.
renameStreets :: (String -> String) -> Person -> Person
