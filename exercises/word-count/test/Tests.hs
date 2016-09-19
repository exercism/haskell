{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Data.Map          (fromList)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import WordCount (wordCount)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "word-count" $
          describe "wordCount" $ for_ cases test
  where

    test Case{..} = it description $ returnedMap `shouldBe` expectedMap
      where
        returnedMap = wordCount input
        expectedMap = fromIntegral <$> fromList expected

-- Test cases adapted from `exercism/x-common/word-count.json` on 2016-07-26.

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: [(String, Integer)]
                 }

cases :: [Case]
cases = [ Case { description = "count one word"
               , input       = "word"
               , expected    = [ ("word", 1) ]
               }
        , Case { description = "count one of each word"
               , input       = "one of each"
               , expected    = [ ("one" , 1)
                               , ("of"  , 1)
                               , ("each", 1) ]
               }
        , Case { description = "multiple occurrences of a word"
               , input       = "one fish two fish red fish blue fish"
               , expected    = [ ("one" , 1)
                               , ("fish", 4)
                               , ("two" , 1)
                               , ("red" , 1)
                               , ("blue", 1) ]
               }
        , Case { description = "ignore punctuation"
               , input       = "car : carpet as java : javascript!!&@$%^&"
               , expected    = [ ("car"       , 1)
                               , ("carpet"    , 1)
                               , ("as"        , 1)
                               , ("java"      , 1)
                               , ("javascript", 1) ]
               }
        , Case { description = "include numbers"
               , input       = "testing, 1, 2 testing"
               , expected    = [ ("testing", 2)
                               , ("1"      , 1)
                               , ("2"      , 1) ]
               }
        , Case { description = "normalize case"
               , input       = "go Go GO Stop stop"
               , expected    = [ ("go"  , 3)
                               , ("stop", 2) ]
               }
        ]
