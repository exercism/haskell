{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

import Data.Foldable       (for_)
import Test.Hspec          (Spec, describe, it, shouldBe, pending)
import Test.Hspec.Runner   (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck     ( Gen
                           , elements
                           , forAll
                           , forAllShrink
                           , arbitraryASCIIChar
                           , suchThat
                           , Testable
                           , Property
                           )
import Data.Maybe          (isNothing, isJust, fromMaybe)
import Data.Char           (ord, isLetter, isPrint)
import Data.List           (isSuffixOf)
import Diamond (diamond)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "diamond" $ do
  it "should not have a result for a non-alpha character" $
    forAllShrink genNonAlphaChar shrinkNonAlphaChar $
      isNothing . diamond

  it "should produce a value for an alpha character" $
    forAll genAlphaChar $ isJust . diamond

  it "should have an odd number of rows" $
    forAllDiamond $ odd . length

  it "should have equal top and bottom" $
    forAllDiamond topEqualToBottom

  it "should have the correct middle row" $
    forAllCharDiamond checkMiddle

  it "should have the same width and height" $
    forAllCharDiamond checkCorrectDimensions

  it "rows should start and end with the same letter" $
    forAllDiamond $
      let headEqualsLast ys = not (null ys) && take 1 ys `isSuffixOf` ys
      in \case [] -> False
               xs -> all headEqualsLast $ filter isLetter <$> xs

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
genAlphaChar = elements ['A'..'Z']

genDiamond :: Gen (Char, Maybe [String])
genDiamond = do
  c <- genAlphaChar
  return (c, diamond c)

forAllDiamond :: Testable prop => ([String] -> prop) -> Property
forAllDiamond = forAllCharDiamond . const

forAllCharDiamond :: Testable prop => (Char -> [String] -> prop) -> Property
forAllCharDiamond p = forAll genDiamond $ uncurry p . fmap (fromMaybe [""])

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
shrinkNonAlphaChar c =
  if isPrint c
   then takeWhile (/= c) printableChars
   else printableChars
  where
    printableChars = filter isPrint ['\0' .. '\127']
