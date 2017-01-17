{-# LANGUAGE Rank2Types #-}

module Person where

import Control.Applicative (Const(..), getConst)
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Time.Calendar (Day, toGregorian, fromGregorian)

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

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a
type Getting s a = (a -> Const a a) -> s -> Const a s
type Setting s t a b = (a -> Identity b) -> s -> Identity t

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = fmap (sbt s) (afb (sa s))

view :: Getting s a -> s -> a
view l = getConst . l Const

over :: Setting s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

set :: Setting s t a b -> b -> s -> t
set l v = over l (const v)

-- boilerplate lenses:

born :: Lens' Person Born
born = lens _born (\p b -> p { _born = b })

address :: Lens' Person Address
address = lens _address (\p a -> p { _address = a })

bornAt :: Lens' Born Address
bornAt = lens _bornAt (\b a -> b { _bornAt = a })

bornOn :: Lens' Born Day
bornOn = lens _bornOn (\b d -> b { _bornOn = d })

street :: Lens' Address String
street = lens _street (\a s -> a { _street = s })

month :: Lens' Day Int
month = lens getMonth setMonth
  where getMonth day   = let (_, m, _) = toGregorian day in m
        setMonth day m = let (y, _, d) = toGregorian day in fromGregorian y m d

bornStreet :: Born -> String
bornStreet = view (bornAt . street)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet = set (address . street)

setBirthMonth :: Int -> Person -> Person
setBirthMonth = set (born . bornOn . month)

renameStreets :: (String -> String) -> Person -> Person
renameStreets f = over birthStreet f . over currentStreet f
  where
    birthStreet = born . bornAt . street
    currentStreet = address . street
