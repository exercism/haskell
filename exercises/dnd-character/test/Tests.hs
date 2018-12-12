{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck
import Text.Printf       (printf)

import DND (Character(..), ability, modifier, character)

data Case = Case { input    :: Int
                 , expected :: Int
                 }

type Ability = Character -> Int

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
  describe "modifier" $ for_ cases test

  describe "ability" $ it "generates values within range" $
    property (forAll ability abilityWithinRange)

  describe "character" $ it "generates valid characters" $
    property (forAll character characterIsValid)
  where
    test Case{..} = it description assertion
      where
        description = printf "computes the modifier for %2d to be %2d" input expected
        assertion = modifier input `shouldBe` expected

    cases :: [Case]
    cases = map (uncurry Case)
      [ (18, 4), (17, 3), (16, 3), (15, 2), (14, 2)
      , (13, 1), (12, 1), (11, 0), (10, 0), (9, -1)
      , (8, -1), (7, -2), (6, -2), (5, -3), (4, -3)
      , (3, -4) ]

    abilityWithinRange :: Int -> Bool
    abilityWithinRange ability' = ability' >= 3 && ability' <= 18

    characterIsValid :: Character -> Property
    characterIsValid character' =
      conjoin $
        hitpointsAddUp character' :
        map (abilityWithinRange' character') abilities
      where
        abilities = [ ("strength", strength)
                    , ("dexterity", dexterity)
                    , ("constitution", constitution)
                    , ("intelligence", intelligence)
                    , ("wisdom", wisdom)
                    , ("charisma", charisma) ]

    abilityWithinRange' :: Character -> (String, Ability) -> Property
    abilityWithinRange' character' (name, ability') =
      counterexample msg (abilityWithinRange (ability' character'))
      where msg = "The '" ++ name ++ "' ability is out of range"

    hitpointsAddUp :: Character -> Property
    hitpointsAddUp Character{..} =
      counterexample msg (hitpoints === 10 + modifier constitution)
      where msg = "The 'hitpoints' are not 10 + constitution modifier"
