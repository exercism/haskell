{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Transpose (transpose)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "responseFor" $ for_ cases test
  where
    test Case{..} = it description assertion
      where
        assertion = transpose lines `shouldBe` expected

data Case = Case { description :: String
                 , lines       :: [String]
                 , expected    :: [String]
                 }

cases :: [Case]
cases = [ Case { description = "empty string"
               , lines = []
               , expected = []
               }
        , Case { description = "two characters in a row"
               , lines = [ "A1" ]
               , expected = [ "A"
                            , "1"
                            ]
               }
        , Case { description = "two characters in a column"
               , lines = [ "A"
                         , "1"
                         ]
               , expected = [ "A1" ]
               }
        , Case { description = "simple"
               , lines = [ "ABC" 
                         , "123" 
                         ]
               , expected = [ "A1" 
                            , "B2"
                            , "C3"
                            ]
               }
        , Case { description = "single line"
               , lines = [ "Single line." ]
               , expected = [ "S"
                            , "i"
                            , "n"
                            , "g"
                            , "l"
                            , "e"
                            , " "
                            , "l"
                            , "i"
                            , "n"
                            , "e"
                            , "."
                            ]
               }
        , Case { description = "first line longer than second line"
               , lines = [ "The fourth line."
                         , "The fifth line."
                         ]
               , expected = [ "TT"
                            , "hh"
                            , "ee"
                            , "  "
                            , "ff"
                            , "oi"
                            , "uf"
                            , "rt"
                            , "th"
                            , "h "
                            , " l"
                            , "li"
                            , "in"
                            , "ne"
                            , "e."
                            , "."
                            ]
               }
        , Case { description = "second line longer than first line"
               , lines = [ "The first line."
                         , "The second line."
                         ]
               , expected = [ "TT"
                            , "hh"
                            , "ee"
                            , "  "
                            , "fs"
                            , "ie"
                            , "rc"
                            , "so"
                            , "tn"
                            , " d"
                            , "l "
                            , "il"
                            , "ni"
                            , "en"
                            , ".e"
                            , " ."
                            ]
               }
        , Case { description = "mixed line length"
               , lines = [ "The longest line."
                         , "A long line."
                         , "A longer line."
                         , "A line."
                         ]
               , expected = [ "TAAA"
                            , "h   "
                            , "elll"
                            , " ooi"
                            , "lnnn"
                            , "ogge"
                            , "n e."
                            , "glr"
                            , "ei "
                            , "snl"
                            , "tei"
                            , " .n"
                            , "l e"
                            , "i ."
                            , "n"
                            , "e"
                            , "."
                            ]
               }
        , Case { description = "square"
               , lines = [ "HEART"
                         , "EMBER"
                         , "ABUSE"
                         , "RESIN"
                         , "TREND"
                         ]
               , expected = [ "HEART"
                            , "EMBER"
                            , "ABUSE"
                            , "RESIN"
                            , "TREND"
                            ]
               }
        , Case { description = "rectangle"
               , lines = [ "FRACTURE"
                         , "OUTLINED"
                         , "BLOOMING"
                         , "SEPTETTE"
                         ]
               , expected = [ "FOBS"
                            , "RULE"
                            , "ATOP"
                            , "CLOT"
                            , "TIME"
                            , "UNIT"
                            , "RENT"
                            , "EDGE"
                            ]
               }
        , Case { description = "triangle"
               , lines = [ "T"
                         , "EE"
                         , "AAA"
                         , "SSSS"
                         , "EEEEE"
                         , "RRRRRR"
                         ]
               , expected = [ "TEASER"
                            , " EASER"
                            , "  ASER"
                            , "   SER"
                            , "    ER"
                            , "     R"
                            ]
               }
        ]
