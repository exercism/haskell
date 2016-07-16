module FoodChain (song) where

import Data.Array (Array, Ix, (!), listArray)
import Data.Char (toLower)
import Text.Printf (printf)

data Animal = Fly | Spider | Bird | Cat | Dog | Goat | Cow | Horse
              deriving (Eq, Enum, Ix, Ord, Show)

-- | An array that contains the lowercase string representation for all
-- animals. E.g. animalNames ! Fly == "fly"
animalNames :: Array Animal String
animalNames = listArray (Fly, Horse) $ map showLower [Fly ..]
  where showLower a = let (h:t) = show a
                      in (toLower h : t)

song :: String
song = unlines $ map verse [Fly ..]

verse :: Animal -> String
verse animal = swallow animal ++ unlines (map catches prey)
  where prey | animal `elem` [Fly, Horse] = []
             | otherwise =  [animal, pred animal .. Spider]

swallow :: Animal -> String
swallow animal = printf "I know an old lady who swallowed a %s.\n%s\n"
                        (animalNames ! animal) (comment animal)

catches :: Animal -> String
catches animal = printf "She swallowed the %s to catch the %s%s"
                        (animalNames ! animal) (animalNames ! prev) (end prev)
  where prev = pred animal
        end Fly    = ".\n" ++ comment Fly
        end Spider = " that wriggled and jiggled and tickled inside her."
        end _      = "."

comment :: Animal -> String
comment animal = case animal of
  Fly    -> "I don't know why she swallowed the fly. Perhaps she'll die."
  Spider -> "It wriggled and jiggled and tickled inside her."
  Bird   -> "How absurd to swallow a bird!"
  Cat    -> "Imagine that, to swallow a cat!"
  Dog    -> "What a hog, to swallow a dog!"
  Goat   -> "Just opened her throat and swallowed a goat!"
  Cow    -> "I don't know how she swallowed a cow!"
  Horse  -> "She's dead, of course!" 
