{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import StateOfTicTacToe (gameState, GameState(..))

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "measure" $ for_ cases test
  where
    test Case{..} = it description $ gameState board `shouldBe` expected
 
data Case = Case { description :: String
                 , board       :: [String]
                 , expected    :: GameState
                 }

cases :: [Case]
cases = [ Case { description = "Finished game where X won via left column victory"
               , board = ["XOO", "X  ", "X  "]
               , expected = WinX
               }
        , Case { description = "Finished game where X won via middle column victory"
               , board = ["OXO", " X ", " X "]
               , expected = WinX
               }
        , Case { description = "Finished game where X won via right column victory"
               , board = ["OOX", "  X", "  X"]
               , expected = WinX
               }
        , Case { description = "Finished game where O won via left column victory"
               , board = ["OXX", "OX ", "O  "]
               , expected = WinO
               }
        , Case { description = "Finished game where O won via middle column victory"
               , board = ["XOX", " OX", " O "]
               , expected = WinO
               }
        , Case { description = "Finished game where O won via right column victory"
               , board = ["XXO", " XO", "  O"]
               , expected = WinO
               }
        , Case { description = "Finished game where X won via top row victory"
               , board = ["XXX", "XOO", "O  "]
               , expected = WinX
               }
        , Case { description = "Finished game where X won via middle row victory"
               , board = ["O  ", "XXX", " O "]
               , expected = WinX
               }
        , Case { description = "Finished game where X won via bottom row victory"
               , board = [" OO", "O X", "XXX"]
               , expected = WinX
               }
        , Case { description = "Finished game where O won via top row victory"
               , board = ["OOO", "XXO", "XX "]
               , expected = WinO
               }
        , Case { description = "Finished game where O won via middle row victory"
               , board = ["XX ", "OOO", "X  "]
               , expected = WinO
               }
        , Case { description = "Finished game where O won via bottom row victory"
               , board = ["XOX", " XX", "OOO"]
               , expected = WinO
               }
        , Case { description = "Finished game where X won via falling diagonal victory"
               , board = ["XOO", " X ", "  X"]
               , expected = WinX
               }
        , Case { description = "Finished game where X won via rising diagonal victory"
               , board = ["O X", "OX ", "X  "]
               , expected = WinX
               }
        , Case { description = "Finished game where O won via falling diagonal victory"
               , board = ["OXX", "OOX", "X O"]
               , expected = WinO
               }
        , Case { description = "Finished game where O won via rising diagonal victory"
               , board = ["  O", " OX", "OXX"]
               , expected = WinO
               }
        , Case { description = "Finished game where X won via a row and a column victory"
               , board = ["XXX", "XOO", "XOO"]
               , expected = WinX
               }
        , Case { description = "Finished game where X won via two diagonal victories"
               , board = ["XOX", "OXO", "XOX"]
               , expected = WinX
               }
        , Case { description = "Drawn"
               , board = ["XOX", "XXO", "OXO"]
               , expected = Draw
               }
        , Case { description = "Another draw"
               , board = ["XXO", "OXX", "XOO"]
               , expected = Draw
               }
        , Case { description = "Ongoing game: one move in"
               , board = ["   ", "X  ", "   "]
               , expected = Ongoing
               }
        , Case { description = "Ongoing game: two moves in"
               , board = ["O  ", " X ", "   "]
               , expected = Ongoing
               }
        , Case { description = "Ongoing game: five moves in"
               , board = ["X  ", " XO", "OX "]
               , expected = Ongoing
               }
        , Case { description = "Invalid board: X went twice"
               , board = ["XX ", "   ", "   "]
               , expected = Impossible
               }
        , Case { description = "Invalid board: O started"
               , board = ["OOX", "   ", "   "]
               , expected = Impossible
               }
        , Case { description = "Invalid board: X won and O kept playing"
               , board = ["XXX", "OOO", "   "]
               , expected = Impossible
               }
        , Case { description = "Invalid board: players kept playing after a win"
               , board = ["XXX", "OOO", "XOX"]
               , expected = Impossible
               }
        ]
