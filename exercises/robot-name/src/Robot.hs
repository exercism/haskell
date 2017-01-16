module Robot (Robot, mkRobot, resetName, robotName) where

data Robot = Dummy

mkRobot :: IO Robot
mkRobot = error "You need to implement this function."

resetName :: Robot -> IO ()
resetName = error "You need to implement this function."

robotName :: Robot -> IO String
robotName = error "You need to implement this function."
