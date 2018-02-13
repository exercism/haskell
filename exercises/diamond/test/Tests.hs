{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Diamond (diamond)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "diamond" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion = diamond input `shouldBe` Just expected


data Case = Case { description :: String
                 , input       :: Char
                 , expected    :: [String]
                 }

cases :: [Case]
cases = [ Case { description = "Degenerate case with a single 'A' row"
               , input       = 'A'
               , expected    = ["A"]
               }
        , Case { description = "Degenerate case with no row containing 3 distinct groups of spaces"
               , input       = 'B'
               , expected    = [" A ",
                                "B B",
                                " A "]
               }
        , Case { description = "Smallest non-degenerate case with odd diamond side length"
               , input       = 'C'
               , expected    = ["  A  ",
                                " B B ",
                                "C   C",
                                " B B ",
                                "  A  "]
               }
        , Case { description = "Smallest non-degenerate case with even diamond side length"
               , input       = 'D'
               , expected    = ["   A   ",
                                "  B B  ",
                                " C   C ",
                                "D     D",
                                " C   C ",
                                "  B B  ",
                                "   A   "]
               }
        , Case { description = "Largest possible diamond"
               , input       = 'Z'
               , expected    = [
                        "                         A                         ",
                        "                        B B                        ",
                        "                       C   C                       ",
                        "                      D     D                      ",
                        "                     E       E                     ",
                        "                    F         F                    ",
                        "                   G           G                   ",
                        "                  H             H                  ",
                        "                 I               I                 ",
                        "                J                 J                ",
                        "               K                   K               ",
                        "              L                     L              ",
                        "             M                       M             ",
                        "            N                         N            ",
                        "           O                           O           ",
                        "          P                             P          ",
                        "         Q                               Q         ",
                        "        R                                 R        ",
                        "       S                                   S       ",
                        "      T                                     T      ",
                        "     U                                       U     ",
                        "    V                                         V    ",
                        "   W                                           W   ",
                        "  X                                             X  ",
                        " Y                                               Y ",
                        "Z                                                 Z",
                        " Y                                               Y ",
                        "  X                                             X  ",
                        "   W                                           W   ",
                        "    V                                         V    ",
                        "     U                                       U     ",
                        "      T                                     T      ",
                        "       S                                   S       ",
                        "        R                                 R        ",
                        "         Q                               Q         ",
                        "          P                             P          ",
                        "           O                           O           ",
                        "            N                         N            ",
                        "             M                       M             ",
                        "              L                     L              ",
                        "               K                   K               ",
                        "                J                 J                ",
                        "                 I               I                 ",
                        "                  H             H                  ",
                        "                   G           G                   ",
                        "                    F         F                    ",
                        "                     E       E                     ",
                        "                      D     D                      ",
                        "                       C   C                       ",
                        "                        B B                        ",
                        "                         A                         "]
               }
        ]
