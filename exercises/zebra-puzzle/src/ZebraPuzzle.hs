module ZebraPuzzle (Resident(..), Solution(..), solve) where

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

solve :: Solution
solve = error "You need to implement this function."

