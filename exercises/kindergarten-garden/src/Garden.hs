module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.Map (Map)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

defaultGarden :: String -> Map String [Plant]
defaultGarden = error "You need to implement this function."

garden :: [String] -> String -> Map String [Plant]
garden = error "You need to implement this function."

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants = error "You need to implement this function."
