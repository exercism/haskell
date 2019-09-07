{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck   (Gen, elements, forAll, choose)
import Data.List         ((\\))
import Data.Maybe        (isNothing, fromMaybe)
import Data.Char         (ord)
import Diamond (diamond)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "diamond" $ do
  it "non-Alpha characters should produce `Nothing`" $
    forAll genNonAlphaChar $
      isNothing . diamond

  it "Length of a diamond should be odd" $
    forAll genAlphaChar $ odd . length . fromMaybe [""] . diamond

  it "Top and bottom of a diamond should be equal" $
    forAll genAlphaChar $ topEqualToBottom . fromMaybe [""] . diamond

  it "Check the middle of the diamond is correct" $
    forAll genAlphaChar $ \c -> checkMiddle c . fromMaybe [""] $ diamond c

  it "Check the dimensionality of the diamond is correct" $
    forAll genAlphaChar $ \c -> checkCorrectDimensions c . fromMaybe [""] $ diamond c

  for_ cases test
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

genNonAlphaChar :: Gen Char
genNonAlphaChar = elements nonAlphaChars
  where nonAlphaChars = ['\0' .. '\127'] \\ (['A' .. 'Z'] ++ ['a' .. 'z'])

genAlphaChar :: Gen Char
genAlphaChar = choose ('A', 'Z')

topLength :: [String] -> Int
topLength = (`div` 2) . length

position :: Char -> Int
position c = ord c - ord 'A'

topEqualToBottom :: [String] -> Bool
topEqualToBottom xs = take len xs == take len (reverse xs)
  where
    len = topLength xs

checkMiddle :: Char -> [String] -> Bool
checkMiddle c xs = getMiddle xs == middle
  where
    getMiddle ys = ys !! topLength ys
    leftSide = c : replicate (position c) ' '
    rightSide = tail . reverse $ leftSide
    middle = leftSide ++ rightSide

checkCorrectDimensions :: Char -> [String] -> Bool
checkCorrectDimensions c xs = length xs == dim && all ((== dim) . length) xs
  where
    dim = 2 * position c + 1
