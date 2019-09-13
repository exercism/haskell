{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe, pending)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck   (Gen, elements, forAll, forAllShrink, arbitraryASCIIChar, suchThat)
import Data.List         (partition)
import Data.Maybe        (isNothing, isJust, fromMaybe)
import Data.Char         (ord, isLetter, isUpper, isPrint)
import Diamond (diamond)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "diamond" $ do
  it "should not have a result for a non-alpha character" $
    forAllShrink genNonAlphaChar shrinkNonAlphaChar $
      isNothing . diamond

  it "should produce a value for an alpha character" $
    forAll genAlphaChar $
      isJust . diamond

  it "should have an odd number of rows" $
    forAll genAlphaChar $ odd . length . fromMaybe [""] . diamond

  it "should have equal top and bottom" $
    forAll genAlphaChar $ topEqualToBottom . fromMaybe [""] . diamond

  it "should have the correct middle row" $
    forAll genAlphaChar $ \c -> checkMiddle c . fromMaybe [""] $ diamond c

  it "should have the same width and height" $
    forAll genAlphaChar $ \c -> checkCorrectDimensions c . fromMaybe [""] $ diamond c

  it "rows should start and end with the same character" $
    pending

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
genNonAlphaChar = arbitraryASCIIChar `suchThat` (not . isLetter)

genAlphaChar :: Gen Char
genAlphaChar = arbitraryASCIIChar `suchThat` isUpper

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

shrinkNonAlphaChar :: Char -> String
shrinkNonAlphaChar c = if isPrint c
                        then takeWhile (/= c) printableChars
                        else printableChars
  where
    (printableChars, _) = partition isPrint ['\0' .. '\127']
