{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Data.List (intercalate)

import Proverb (recite)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "recite" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion = recite input `shouldBe` expected


data Case = Case { description :: String
                 , input       :: [String]
                 , expected    :: String
                 }

joinLines :: [String] -> String
joinLines = intercalate "\n"

cases :: [Case]
cases = [ Case { description = "zero pieces"
               , input       = []
               , expected    = ""
               }
        , Case { description = "one piece"
               , input       = ["nail"]
               , expected    = "And all for the want of a nail."
               }
        , Case { description = "two pieces"
               , input       = ["nail", "shoe"]
               , expected    =
                   joinLines [ "For want of a nail the shoe was lost."
                             , "And all for the want of a nail."
                             ]
               }
        , Case { description = "three pieces"
               , input       = ["nail", "shoe", "horse"]
               , expected    =
                   joinLines [ "For want of a nail the shoe was lost."
                             , "For want of a shoe the horse was lost."
                             , "And all for the want of a nail."
                             ]
               }
        , Case { description = "full proverb"
               , input       = [ "nail"
                               , "shoe"
                               , "horse"
                               , "rider"
                               , "message"
                               , "battle"
                               , "kingdom"
                               ]
               , expected    =
                 joinLines [ "For want of a nail the shoe was lost."
                           , "For want of a shoe the horse was lost."
                           , "For want of a horse the rider was lost."
                           , "For want of a rider the message was lost."
                           , "For want of a message the battle was lost."
                           , "For want of a battle the kingdom was lost."
                           , "And all for the want of a nail."
                           ]
               }
        , Case { description = "four pieces modernized"
               , input       = ["pin", "gun", "soldier", "battle"]
               , expected    =
                 joinLines [ "For want of a pin the gun was lost."
                           , "For want of a gun the soldier was lost."
                           , "For want of a soldier the battle was lost."
                           , "And all for the want of a pin."
                           ]
               }
        ]
