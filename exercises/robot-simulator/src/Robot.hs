module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

bearing :: Robot -> Bearing
bearing = undefined

coordinates :: Robot -> (Integer, Integer)
coordinates = undefined

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = undefined

simulate :: Robot -> String -> Robot
simulate = undefined

turnLeft :: Bearing -> Bearing
turnLeft = undefined

turnRight :: Bearing -> Bearing
turnRight = undefined
