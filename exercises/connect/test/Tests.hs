{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Connect (Color(Black,White), resultFor)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "connect" $
          describe "resultFor" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion  = resultFor testBoard `shouldBe` expected
        testBoard  = filter (/=' ') <$> board

-- Test cases adapted from `exercism/x-common` on 2016-09-16.

data Case = Case { description :: String
                 , board       :: [String]
                 , expected    :: Maybe Color
                 }

cases :: [Case]
cases = [ Case { description = "an empty board has no winner"
               , board       = [ ". . . . ."
                               , " . . . . ."
                               , "  . . . . ."
                               , "   . . . . ."
                               , "    . . . . ." ]
               , expected    = Nothing
               }
        , Case { description = "X can win on a 1x1 board"
               , board       = [ "X" ]
               , expected    = Just Black
               }
        , Case { description = "O can win on a 1x1 board"
               , board       = [ "O" ]
               , expected    = Just White
               }
        , Case { description = "only edges does not make a winner"
               , board       = [ "O O O X"
                               , " X . . X"
                               , "  X . . X"
                               , "   X O O O" ]
               , expected    = Nothing
               }
        , Case { description = "illegal diagonal does not make a winner"
               , board       = [ "X O . ."
                               , " O X X X"
                               , "  O X O ."
                               , "   . O X ."
                               , "    X X O O" ]
               , expected    = Nothing
               }
        , Case { description = "nobody wins crossing adjacent angles"
               , board       = [ "X . . ."
                               , " . X O ."
                               , "  O . X O"
                               , "   . O . X"
                               , "    . . O ." ]
               , expected    = Nothing
               }
        , Case { description = "X wins crossing from left to right"
               , board       = [ ". O . ."
                               , " O X X X"
                               , "  O X O ."
                               , "   X X O X"
                               , "    . O X ." ]
               , expected    = Just Black
               }
        , Case { description = "O wins crossing from top to bottom"
               , board       = [ ". O . ."
                               , " O X X X"
                               , "  O O O ."
                               , "   X X O X"
                               , "    . O X ." ]
               , expected    = Just White
               }
        , Case { description = "X wins using a convoluted path"
               , board       = [ ". X X . ."
                               , " X . X . X"
                               , "  . X . X ."
                               , "   . X X . ."
                               , "    O O O O O" ]
               , expected    = Just Black
               }
        , Case { description = "X wins using a spiral path"
               , board       = [ "O X X X X X X X X"
                               , " O X O O O O O O O"
                               , "  O X O X X X X X O"
                               , "   O X O X O O O X O"
                               , "    O X O X X X O X O"
                               , "     O X O O O X O X O"
                               , "      O X X X X X O X O"
                               , "       O O O O O O O X O"
                               , "        X X X X X X X X O" ]
               , expected    = Just Black
               }
        ]
