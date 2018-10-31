module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
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
bearing robot = error "You need to implement this function."

coordinates :: Robot -> (Integer, Integer)
coordinates robot = error "You need to implement this function."

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = error "You need to implement this function."

move :: Robot -> String -> Robot
move robot instructions = error "You need to implement this function."

turnLeft :: Bearing -> Bearing
turnLeft direction = error "You need to implement this function."

turnRight :: Bearing -> Bearing
turnRight direction = error "You need to implement this function."
