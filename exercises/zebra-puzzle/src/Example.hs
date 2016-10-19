module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Control.Monad (guard)
import Data.List (nub, find)
import Data.Maybe (fromJust)

data Color = Red | Green | Yellow | Blue | Ivory
  deriving (Eq, Show, Enum)
data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Enum)
data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show, Enum)
data Beverage = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show, Enum)
data Cigarette = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show, Enum)
data Position = One | Two | Three | Four | Five
  deriving (Eq, Show, Enum)

data House = House { position :: Position
                   , color :: Color
                   , resident :: Resident
                   , beverage :: Beverage
                   , cigarette :: Cigarette
                   , pet :: Pet
                   } deriving (Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

solve :: Solution
solve = Solution waterDrinker' zebraOwner'
  where
    waterDrinker' = residentWith beverage Water
    zebraOwner'   = residentWith pet Zebra
    residentWith :: (Eq a) => (House -> a) -> a -> Resident
    residentWith what value = resident $ houseWith what value fiveHouses

fiveHouses :: [House]
fiveHouses = head $ do
  one   <- housesAtPosition One
  two   <- housesAtPosition Two
  guard $ uniqueHouses [one, two]   -- prune search tree as early as possible
  three <- housesAtPosition Three
  guard $ uniqueHouses [one, two, three]
  four  <- housesAtPosition Four
  guard $ uniqueHouses [one, two, three, four]
  five  <- housesAtPosition Five
  let candidates = [one, two, three, four, five]
  guard $ uniqueHouses candidates
  guard $ validPositions candidates
  return candidates
  where
    housesAtPosition :: Position -> [House]
    housesAtPosition pos = filter ((== pos) . position) validHouses

validHouses :: [House]
validHouses = do
  position' <- [One .. Five]
  color' <- [Red .. Ivory]
  resident' <- [Englishman .. Japanese]
  beverage' <- [Coffee .. Water]
  cigarette' <- [OldGold .. Parliaments]
  pet' <- [Dog .. Zebra]
  let house = House position' color' resident' beverage' cigarette' pet'
  guard $ validHouse house
  return house

validHouse :: House -> Bool
validHouse (House position' color' resident' beverage' cigarette' pet') =
  all (uncurry iff) [
    (color' == Red, resident' == Englishman),
    (resident' == Spaniard, pet' == Dog),
    (color' == Green, beverage' == Coffee),
    (resident' == Ukrainian, beverage' == Tea),
    (cigarette' == OldGold, pet' == Snails),
    (color' == Yellow, cigarette' == Kools),
    (position' == Three, beverage' == Milk),
    (position' == One, resident' == Norwegian),
    (beverage' == OrangeJuice, cigarette' == LuckyStrike),
    (resident' == Japanese, cigarette' == Parliaments)
  ]

iff :: Bool -> Bool -> Bool
iff True  True  = True
iff False False = True
iff _     _     = False

uniqueHouses :: [House] -> Bool
uniqueHouses houses =
  unique color && unique resident && unique beverage &&
  unique cigarette && unique pet
  where
    unique :: (Eq a) => (House -> a) -> Bool
    unique what = (== length houses) . length . nub $ map what houses

houseWith :: (Eq a) => (House -> a) -> a -> [House] -> House
houseWith what value = fromJust . find ((== value) . what)

validPositions :: [House] -> Bool
validPositions houses =
  houseWith' color Green `toTheRight` houseWith' color Ivory &&
  houseWith' cigarette Chesterfields `nextTo` houseWith' pet Fox &&
  houseWith' cigarette Kools `nextTo` houseWith' pet Horse &&
  houseWith' resident Norwegian `nextTo` houseWith' color Blue
  where
    houseWith' what value = houseWith what value houses
    toTheRight :: House -> House -> Bool
    toTheRight h1 h2 = fromEnum (position h1) == fromEnum (position h2) + 1
    nextTo :: House -> House -> Bool
    nextTo h1 h2 = abs (fromEnum (position h1) - fromEnum (position h2)) == 1

