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
defaultGarden = undefined

garden :: [String] -> String -> Map String [Plant]
garden = undefined

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants = undefined
