{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Acronym (abbreviate)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "abbreviate" $ for_ cases test
  where
    test Case {..} = it description $ abbreviate input `shouldBe` expected

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: String
                 }

cases :: [Case]
cases = [ Case { description = "basic"
               , input       = "Portable Network Graphics"
               , expected    = "PNG"
               }
        , Case { description = "lowercase words"
               , input       = "Ruby on Rails"
               , expected    = "ROR"
               }
        -- Although this case was removed in specification 1.1.0,
        -- the Haskell track has chosen to keep it,
        -- since it makes the problem more interesting.
        , Case { description = "camelcase"
               , input       = "HyperText Markup Language"
               , expected    = "HTML"
               }
        , Case { description = "punctuation"
               , input       = "First In, First Out"
               , expected    = "FIFO"
               }
        , Case { description = "all caps word"
               , input       = "GNU Image Manipulation Program"
               , expected    = "GIMP"
               }
        , Case { description = "punctuation without whitespace"
               , input       = "Complementary metal-oxide semiconductor"
               , expected    = "CMOS"
               }
        , Case { description = "very long abbreviation"
               , input       = "Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me"
               , expected    = "ROTFLSHTMDCOALM"
               }
        ]
