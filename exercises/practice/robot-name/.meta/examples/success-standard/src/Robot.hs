module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import           Control.Concurrent.MVar (MVar, newMVar, putMVar, readMVar,
                                          takeMVar)
import           Control.Monad.State     (StateT, get, modify)
import           Control.Monad.Trans     (lift)
import           Data.Set                (Set)
import qualified Data.Set                as S
import           System.Random           (randomRIO)

newtype Robot = Robot { robotNameVar :: MVar String }
type RunState = Set String

initialState :: RunState
initialState = S.empty

randomName :: IO String
randomName = mapM randomRIO [letter, letter, digit, digit, digit]
  where
    letter = ('A', 'Z')
    digit = ('0', '9')

randomUniqueName :: StateT RunState IO String
randomUniqueName = do
  name <- lift randomName
  nameSpace <- get
  if name `S.member` nameSpace
    then randomUniqueName
    else do
      modify (S.insert name)
      return name

mkRobot :: StateT RunState IO Robot
mkRobot = fmap Robot $ randomUniqueName >>= lift . newMVar

resetName :: Robot -> StateT RunState IO ()
resetName (Robot name) = do
  oldName <- lift $ takeMVar name
  randomUniqueName >>= lift . putMVar name
  modify (S.delete oldName)


robotName :: Robot -> IO String
robotName = readMVar . robotNameVar
