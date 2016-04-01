{-# LANGUAGE TemplateHaskell #-}

module Person where

import           Control.Lens
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

makeLenses ''Person
makeLenses ''Name
makeLenses ''Born
makeLenses ''Address
makeLenses ''Gregorian

gregorianDay :: Iso' Gregorian Day
gregorianDay = iso toDay fromDay
  where
    toDay (Gregorian y m d) = fromGregorian y m d
    fromDay d' = let (y, m, d) = toGregorian d' in Gregorian y m d

bornStreet :: Born -> String
bornStreet = view (bornAt . street)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet = set (address . street)

setBirthMonth :: Int -> Person -> Person
setBirthMonth = set (born . bornOn . from gregorianDay . month)

renameStreets :: (String -> String) -> Person -> Person
renameStreets f = over birthStreet f . over currentStreet f
  where
    birthStreet = born . bornAt . street
    currentStreet = address . street
