{-# LANGUAGE RecordWildCards #-}
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
modifier a = (a - 10) `div` 2

ability :: Gen Int
ability = do
  ds <- replicateM 4 (choose (1, 6))
  return (sum ds - minimum ds)

character :: Gen Character
character = do
  let name = "Bob"
  strength <- ability
  dexterity <- ability
  constitution <- ability
  intelligence <- ability
  wisdom <- ability
  charisma <- ability
  let hitpoints = 10 + modifier constitution
  return Character{..}
