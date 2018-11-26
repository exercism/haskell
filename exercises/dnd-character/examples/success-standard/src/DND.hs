module DND (Character(..), ability, modifier, character) where

import Control.Monad (replicateM)
import Test.QuickCheck.Gen (Gen, choose)

data Character = Character
  { name         :: String
  , strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier ability' = (ability' - 10) `div` 2

ability :: Gen Int
ability = do
  ds <- replicateM 4 (choose (1, 6))
  return (sum ds - minimum ds)

character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  int <- ability
  wis <- ability
  cha <- ability
  let hp = 10 + modifier con
  return $ Character "Bob" str dex con int wis cha hp
