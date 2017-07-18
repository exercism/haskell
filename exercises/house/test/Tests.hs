import Control.Monad     (unless)
import Test.Hspec        (Spec, describe, expectationFailure, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import House (rhyme)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "rhyme" $ do

          -- First we test the input, line by line, to give more
          -- useful error messages.

          it "matches lines" $ sequence_ lineAssertions

          -- Finally, because testing lines we are unable
          -- to detect a missing newline at the end of the
          -- lyrics, we test the full song.

          it "matches full song" $ rhyme `shouldBe` lyrics
  where

    lineAssertions = zipWith checkLine [1 :: Int ..] $ zipMaybe (lines rhyme) (lines lyrics)

    checkLine lineno (got, want) =
      unless (got == want) $
        expectationFailure $ "mismatch at line " ++ show lineno ++ "\nexpected: " ++ show want ++ "\n but got: " ++ show got

    zipMaybe    []     []  = []
    zipMaybe (x:xs)    []  = (Just x , Nothing) : zipMaybe xs []
    zipMaybe    []  (y:ys) = (Nothing, Just y ) : zipMaybe [] ys
    zipMaybe (x:xs) (y:ys) = (Just x , Just y ) : zipMaybe xs ys

-- Lyrics extracted from `exercism/problem-specifications` on 2016-09-23.

lyrics :: String
lyrics = "This is the house that Jack built.\n\
         \\n\
         \This is the malt\n\
         \that lay in the house that Jack built.\n\
         \\n\
         \This is the rat\n\
         \that ate the malt\n\
         \that lay in the house that Jack built.\n\
         \\n\
         \This is the cat\n\
         \that killed the rat\n\
         \that ate the malt\n\
         \that lay in the house that Jack built.\n\
         \\n\
         \This is the dog\n\
         \that worried the cat\n\
         \that killed the rat\n\
         \that ate the malt\n\
         \that lay in the house that Jack built.\n\
         \\n\
         \This is the cow with the crumpled horn\n\
         \that tossed the dog\n\
         \that worried the cat\n\
         \that killed the rat\n\
         \that ate the malt\n\
         \that lay in the house that Jack built.\n\
         \\n\
         \This is the maiden all forlorn\n\
         \that milked the cow with the crumpled horn\n\
         \that tossed the dog\n\
         \that worried the cat\n\
         \that killed the rat\n\
         \that ate the malt\n\
         \that lay in the house that Jack built.\n\
         \\n\
         \This is the man all tattered and torn\n\
         \that kissed the maiden all forlorn\n\
         \that milked the cow with the crumpled horn\n\
         \that tossed the dog\n\
         \that worried the cat\n\
         \that killed the rat\n\
         \that ate the malt\n\
         \that lay in the house that Jack built.\n\
         \\n\
         \This is the priest all shaven and shorn\n\
         \that married the man all tattered and torn\n\
         \that kissed the maiden all forlorn\n\
         \that milked the cow with the crumpled horn\n\
         \that tossed the dog\n\
         \that worried the cat\n\
         \that killed the rat\n\
         \that ate the malt\n\
         \that lay in the house that Jack built.\n\
         \\n\
         \This is the rooster that crowed in the morn\n\
         \that woke the priest all shaven and shorn\n\
         \that married the man all tattered and torn\n\
         \that kissed the maiden all forlorn\n\
         \that milked the cow with the crumpled horn\n\
         \that tossed the dog\n\
         \that worried the cat\n\
         \that killed the rat\n\
         \that ate the malt\n\
         \that lay in the house that Jack built.\n\
         \\n\
         \This is the farmer sowing his corn\n\
         \that kept the rooster that crowed in the morn\n\
         \that woke the priest all shaven and shorn\n\
         \that married the man all tattered and torn\n\
         \that kissed the maiden all forlorn\n\
         \that milked the cow with the crumpled horn\n\
         \that tossed the dog\n\
         \that worried the cat\n\
         \that killed the rat\n\
         \that ate the malt\n\
         \that lay in the house that Jack built.\n\
         \\n\
         \This is the horse and the hound and the horn\n\
         \that belonged to the farmer sowing his corn\n\
         \that kept the rooster that crowed in the morn\n\
         \that woke the priest all shaven and shorn\n\
         \that married the man all tattered and torn\n\
         \that kissed the maiden all forlorn\n\
         \that milked the cow with the crumpled horn\n\
         \that tossed the dog\n\
         \that worried the cat\n\
         \that killed the rat\n\
         \that ate the malt\n\
         \that lay in the house that Jack built.\n"
