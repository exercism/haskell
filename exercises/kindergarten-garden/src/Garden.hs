module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.Map (Map)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

garden :: [String] -> String -> Map String [Plant]
garden students plants = error "You need to implement this function."

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants student garden = error "You need to implement this function."
