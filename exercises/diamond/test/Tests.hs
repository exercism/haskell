{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Char               (isLetter, isPrint, isSpace)
import Data.Foldable           (for_)
import Data.List               (isSuffixOf)
import Data.Maybe              (isJust, isNothing)
import Data.String.Conversions (convertString)
import Test.Hspec              (Spec, describe, it, shouldBe)
import Test.Hspec.Runner       (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck         (arbitraryASCIIChar, conjoin, counterexample,
                                discard, elements, forAll, forAllShrink, Gen,
                                Property, suchThat, Testable, (===))

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
    forAllDiamond $ \rows ->
      let halfRoundDown = length rows `div` 2
      in take halfRoundDown rows === take halfRoundDown (reverse rows)

  it "should have the same width and height" $
    forAllDiamond $ \rows ->
      let sameHeightWidth idx row = counterexample
            (concat [ "The length of row with index "
                    , show idx
                    , " is not equal to the height" ])
            (length row === length rows)
      in conjoin $ zipWith sameHeightWidth [0 :: Int ..] rows

  it "rows should start and end with the same letter" $
    forAllDiamond $
      let headEqualsLast row = not (null row) && take 1 row `isSuffixOf` row
      in (&&) <$> not . null <*> all (headEqualsLast . filter (not . isSpace))

  for_ cases test
  where
    test Case{..} = it description assertion
      where
        assertion = (fmap . fmap) convertString (diamond input) `shouldBe` Just expected


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

genDiamond :: Gen (Maybe [String])
genDiamond = (fmap . fmap . fmap) convertString $ diamond <$> genAlphaChar

forAllDiamond :: Testable prop => ([String] -> prop) -> Property
forAllDiamond p = forAll genDiamond $ maybe discard p

shrinkNonAlphaChar :: Char -> String
shrinkNonAlphaChar c =
  if isPrint c
  then takeWhile (/= c) printableChars
  else printableChars
  where
    printableChars = filter isPrint ['\0' .. '\127']
