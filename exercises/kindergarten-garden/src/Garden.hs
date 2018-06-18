module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Garden = Dummy

garden :: [String] -> String -> Garden
garden students plants = error "You need to implement this function."

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student garden = error "You need to implement this function."
