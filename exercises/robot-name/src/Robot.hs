module Robot (Robot, mkRobot, resetName, robotName) where

data Robot = Dummy

mkRobot :: IO Robot
mkRobot = undefined

resetName :: Robot -> IO ()
resetName = undefined

robotName :: Robot -> IO String
robotName = undefined
