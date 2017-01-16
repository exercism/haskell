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

data Robot = Dummy

bearing :: Robot -> Bearing
bearing = error "You need to implement this function."

coordinates :: Robot -> (Integer, Integer)
coordinates = error "You need to implement this function."

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = error "You need to implement this function."

simulate :: Robot -> String -> Robot
simulate = error "You need to implement this function."

turnLeft :: Bearing -> Bearing
turnLeft = error "You need to implement this function."

turnRight :: Bearing -> Bearing
turnRight = error "You need to implement this function."
