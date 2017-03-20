module Robot (Robot, mkRobot, resetName, robotName) where

data Robot = Dummy

mkRobot :: IO Robot
mkRobot = error "You need to implement this function."

resetName :: Robot -> IO ()
resetName robot = error "You need to implement this function."

robotName :: Robot -> IO String
robotName robot = error "You need to implement this function."
