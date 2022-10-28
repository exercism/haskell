{-# LANGUAGE OverloadedStrings #-}

import Data.Map          (empty, fromList, lookup, singleton)
import Data.Text         (concat)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Prelude    hiding (concat, lookup)

import Frequency (frequency)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    let odeAnDieFreude = concat             -- Poem by Friedrich Schiller.
          [ "Freude schöner Götterfunken"   -- The corresponding music is
          , "Tochter aus Elysium,"          -- the European Anthem.
          , "Wir betreten feuertrunken,"
          , "Himmlische, dein Heiligtum!"
          , "Deine Zauber binden wieder"
          , "Was die Mode streng geteilt;"
          , "Alle Menschen werden Brüder,"
          , "Wo dein sanfter Flügel weilt."
          ]
        starSpangledBanner = concat         -- American national anthem
          [ "O say can you see by the dawn's early light,"
          , "What so proudly we hailed at the twilight's last gleaming,"
          , "Whose broad stripes and bright stars through the perilous fight,"
          , "O'er the ramparts we watched, were so gallantly streaming?"
          , "And the rockets' red glare, the bombs bursting in air,"
          , "Gave proof through the night that our flag was still there;"
          , "O say does that star-spangled banner yet wave,"
          , "O'er the land of the free and the home of the brave?"
          ]
        wilhelmus = concat                  -- Dutch national anthem
          [ "Wilhelmus van Nassouwe"
          , "ben ik, van Duitsen bloed,"
          , "den vaderland getrouwe"
          , "blijf ik tot in den dood."
          , "Een Prinse van Oranje"
          , "ben ik, vrij, onverveerd,"
          , "den Koning van Hispanje"
          , "heb ik altijd geëerd."
          ]
        anthems = [odeAnDieFreude, starSpangledBanner, wilhelmus]

    describe "frequency" $ do

      it "no texts mean no letters" $
        frequency 1 [] `shouldBe` empty

      it "one letter" $
        frequency 1 ["a"] `shouldBe` singleton 'a' 1

      it "case insensitivity" $
        frequency 1 ["aA"] `shouldBe` singleton 'a' 2

      it "many empty texts still mean no letters" $
        frequency 1 (replicate 10000 "  ") `shouldBe` empty

      it "many times the same text gives a predictable result" $
        frequency 1 (replicate 1000 "abc") `shouldBe` fromList [ ('a', 1000)
                                                               , ('b', 1000)
                                                               , ('c', 1000) ]

      it "punctuation doesn't count" $
        lookup ',' (frequency 1 [odeAnDieFreude]) `shouldBe` Nothing

      it "numbers don't count" $
        lookup '1' (frequency 1 ["Testing, 1, 2, 3"]) `shouldBe` Nothing

      let testAllAnthems n = do
            let frequencies = frequency n anthems
            lookup 'a' frequencies `shouldBe` Just 49
            lookup 't' frequencies `shouldBe` Just 56
            lookup 'ü' frequencies `shouldBe` Just  2

      it "all three anthems, together, 1 worker" $ testAllAnthems 1

      it "all three anthems, together, 4 workers" $ testAllAnthems 4
