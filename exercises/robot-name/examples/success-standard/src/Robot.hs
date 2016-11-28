module Robot (Robot, mkRobot, robotName, resetName) where

import Control.Concurrent (MVar, readMVar, swapMVar, newMVar)
import Control.Monad (void)
import System.Random (randomRIO)

newtype Robot = Robot { robotNameVar :: MVar String }

mkRobot :: IO Robot
mkRobot = fmap Robot $ generateName >>= newMVar

resetName :: Robot -> IO ()
resetName r = void (generateName >>= swapMVar (robotNameVar r))

robotName :: Robot -> IO String
robotName = readMVar . robotNameVar

generateName :: IO String
generateName = mapM randomRIO [ letter, letter, digit, digit, digit ]
  where
    letter = ('A', 'Z')
    digit  = ('0', '9')
