module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import           Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import           Control.Monad           (void)
import           Control.Monad.State     (StateT)
import           Control.Monad.Trans     (lift)
import           System.Random           (randomRIO)

newtype Robot = Robot { robotNameVar :: MVar String }
type RunState = ()

initialState :: RunState
initialState = ()

randomName :: IO String
randomName = mapM randomRIO [letter, letter, digit, digit, digit]
  where
    letter = ('A', 'Z')
    digit = ('0', '9')

mkRobot :: StateT RunState IO Robot
mkRobot = Robot <$> lift (randomName >>= newMVar)

resetName :: Robot -> StateT RunState IO ()
resetName (Robot name) = void . lift $ randomName >>= swapMVar name

robotName :: Robot -> IO String
robotName = readMVar . robotNameVar
