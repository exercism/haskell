{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import WordSearch (search, CharPos(..), WordPos(..))

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs =  describe "search" $ for_ cases test
  where
    test Case{..} = it description $ search grid wordList `shouldBe` expected

data Case = Case { description :: String
                 , grid        :: [String]
                 , wordList    :: [String]
                 , expected    :: [(String, Maybe WordPos)]
                 }

cases :: [Case]
cases = [ Case { description = "Should accept an initial game grid and a target search word"
               , grid        = [ "jefblpepre" ]
               , wordList    = [ "clojure" ]
               , expected    = [ ("clojure", Nothing) ]
               }
        , Case { description = "Should locate one word written left to right"
               , grid        = [ "clojurermt" ]
               , wordList    = [ "clojure" ]
               , expected    = [ ("clojure", Just WordPos{start=CharPos{col=1, row=1}, end=CharPos{col=7, row=1}}) ]
               }
        , Case { description = "Should locate the same word written left to right in a different position"
               , grid        = [ "mtclojurer" ]
               , wordList    = [ "clojure" ]
               , expected    = [ ("clojure", Just WordPos{start=CharPos{col=3, row=1}, end=CharPos{col=9, row=1}}) ]
               }
        , Case { description = "Should locate a different left to right word"
               , grid        = [ "coffeelplx" ]
               , wordList    = [ "coffee" ]
               , expected    = [ ("coffee", Just WordPos{start=CharPos{col=1, row=1}, end=CharPos{col=6, row=1}}) ]
               }
        , Case { description = "Should locate that different left to right word in a different position"
               , grid        = [ "xcoffeezlp" ]
               , wordList    = [ "coffee" ]
               , expected    = [ ("coffee", Just WordPos{start=CharPos{col=2, row=1}, end=CharPos{col=7, row=1}}) ]
               }
        , Case { description = "Should locate a left to right word in three line grid"
               , grid        = [ "camdcimgtc", "jefblpepre","clojurermt" ]
               , wordList    = [ "clojure" ]
               , expected    = [ ("clojure", Just WordPos{start=CharPos{col=1, row=3}, end=CharPos{col=7, row=3}}) ]
               }
        , Case { description = "Should locate a left to right word in ten line grid"
               , grid        = [
                                   "jefblpepre",
                                   "camdcimgtc",
                                   "oivokprjsm",
                                   "pbwasqroua",
                                   "rixilelhrs",
                                   "wolcqlirpc",
                                   "screeaumgr",
                                   "alxhpburyi",
                                   "jalaycalmp",
                                   "clojurermt"
                               ]
               , wordList    = [ "clojure" ]
               , expected    = [ ("clojure", Just WordPos{start=CharPos{col=1, row=10}, end=CharPos{col=7, row=10}}) ]
               }
        , Case { description = "Should locate that left to right word in a different position in a ten line grid"
               , grid        = [
                                   "jefblpepre",
                                   "camdcimgtc",
                                   "oivokprjsm",
                                   "pbwasqroua",
                                   "rixilelhrs",
                                   "wolcqlirpc",
                                   "screeaumgr",
                                   "alxhpburyi",
                                   "clojurermt",
                                   "jalaycalmp"
                               ]
               , wordList    = [ "clojure" ]
               , expected    = [ ("clojure", Just WordPos{start=CharPos{col=1, row=9}, end=CharPos{col=7, row=9}}) ]
               }
        , Case { description = "Should locate a different left to right word in a ten line grid"
               , grid        = [
                                   "jefblpepre",
                                   "camdcimgtc",
                                   "oivokprjsm",
                                   "pbwasqroua",
                                   "rixilelhrs",
                                   "wolcqlirpc",
                                   "fortranftw",
                                   "alxhpburyi",
                                   "clojurermt",
                                   "jalaycalmp"
                               ]
               , wordList    = [ "fortran" ]
               , expected    = [ ("fortran", Just WordPos{start=CharPos{col=1, row=7}, end=CharPos{col=7, row=7}}) ]
               }
        , Case { description = "Should locate multiple words"
               , grid        = [
                                   "jefblpepre",
                                   "camdcimgtc",
                                   "oivokprjsm",
                                   "pbwasqroua",
                                   "rixilelhrs",
                                   "wolcqlirpc",
                                   "fortranftw",
                                   "alxhpburyi",
                                   "jalaycalmp",
                                   "clojurermt"
                               ]
               , wordList    = [ "fortran", "clojure" ]
               , expected    = [
                                   ("fortran", Just WordPos{start=CharPos{col=1, row=7}, end=CharPos{col=7, row=7}}),
                                   ("clojure", Just WordPos{start=CharPos{col=1, row=10}, end=CharPos{col=7, row=10}})
                               ]
               }
        , Case { description = "Should locate a single word written right to left"
               , grid        = [ "rixilelhrs" ]
               , wordList    = [ "elixir" ]
               , expected    = [ ("elixir", Just WordPos{start=CharPos{col=6, row=1}, end=CharPos{col=1, row=1}}) ]
               }
        , Case { description = "Should locate multiple words written in different horizontal directions"
               , grid        = [
                                   "jefblpepre",
                                   "camdcimgtc",
                                   "oivokprjsm",
                                   "pbwasqroua",
                                   "rixilelhrs",
                                   "wolcqlirpc",
                                   "screeaumgr",
                                   "alxhpburyi",
                                   "jalaycalmp",
                                   "clojurermt"
                               ]
               , wordList    = [ "elixir", "clojure" ]
               , expected    = [
                                   ("elixir", Just WordPos{start=CharPos{col=6, row=5}, end=CharPos{col=1, row=5}}),
                                   ("clojure", Just WordPos{start=CharPos{col=1, row=10}, end=CharPos{col=7, row=10}})
                               ]
               }
        , Case { description = "Should locate words written top to bottom"
               , grid        = [
                                   "jefblpepre",
                                   "camdcimgtc",
                                   "oivokprjsm",
                                   "pbwasqroua",
                                   "rixilelhrs",
                                   "wolcqlirpc",
                                   "screeaumgr",
                                   "alxhpburyi",
                                   "jalaycalmp",
                                   "clojurermt"
                               ]
               , wordList    = [ "clojure", "elixir", "ecmascript" ]
               , expected    = [
                                   ("clojure", Just WordPos{start=CharPos{col=1, row=10}, end=CharPos{col=7, row=10}}),
                                   ("elixir", Just WordPos{start=CharPos{col=6, row=5}, end=CharPos{col=1, row=5}}),
                                   ("ecmascript", Just WordPos{start=CharPos{col=10, row=1}, end=CharPos{col=10, row=10}})
                               ]
               }
        , Case { description = "Should locate words written bottom to top"
               , grid        = [
                                   "jefblpepre",
                                   "camdcimgtc",
                                   "oivokprjsm",
                                   "pbwasqroua",
                                   "rixilelhrs",
                                   "wolcqlirpc",
                                   "screeaumgr",
                                   "alxhpburyi",
                                   "jalaycalmp",
                                   "clojurermt"
                               ]
               , wordList    = [ "clojure", "elixir", "ecmascript", "rust" ]
               , expected    = [
                                   ("clojure", Just WordPos{start=CharPos{col=1, row=10}, end=CharPos{col=7, row=10}}),
                                   ("elixir", Just WordPos{start=CharPos{col=6, row=5}, end=CharPos{col=1, row=5}}),
                                   ("ecmascript", Just WordPos{start=CharPos{col=10, row=1}, end=CharPos{col=10, row=10}}),
                                   ("rust", Just WordPos{start=CharPos{col=9, row=5}, end=CharPos{col=9, row=2}})
                               ]
               }
        , Case { description = "Should locate words written top left to bottom right"
               , grid        = [
                                   "jefblpepre",
                                   "camdcimgtc",
                                   "oivokprjsm",
                                   "pbwasqroua",
                                   "rixilelhrs",
                                   "wolcqlirpc",
                                   "screeaumgr",
                                   "alxhpburyi",
                                   "jalaycalmp",
                                   "clojurermt"
                               ]
               , wordList    = [ "clojure", "elixir", "ecmascript", "rust", "java" ]
               , expected    = [
                                   ("clojure", Just WordPos{start=CharPos{col=1, row=10}, end=CharPos{col=7, row=10}}),
                                   ("elixir", Just WordPos{start=CharPos{col=6, row=5}, end=CharPos{col=1, row=5}}),
                                   ("ecmascript", Just WordPos{start=CharPos{col=10, row=1}, end=CharPos{col=10, row=10}}),
                                   ("rust", Just WordPos{start=CharPos{col=9, row=5}, end=CharPos{col=9, row=2}}),
                                   ("java", Just WordPos{start=CharPos{col=1, row=1}, end=CharPos{col=4, row=4}})
                               ]
               }
        , Case { description = "Should locate words written bottom right to top left"
               , grid        = [
                                   "jefblpepre",
                                   "camdcimgtc",
                                   "oivokprjsm",
                                   "pbwasqroua",
                                   "rixilelhrs",
                                   "wolcqlirpc",
                                   "screeaumgr",
                                   "alxhpburyi",
                                   "jalaycalmp",
                                   "clojurermt"
                               ]
               , wordList    = [ "clojure", "elixir", "ecmascript", "rust", "java", "lua" ]
               , expected    = [
                                   ("clojure", Just WordPos{start=CharPos{col=1, row=10}, end=CharPos{col=7, row=10}}),
                                   ("elixir", Just WordPos{start=CharPos{col=6, row=5}, end=CharPos{col=1, row=5}}),
                                   ("ecmascript", Just WordPos{start=CharPos{col=10, row=1}, end=CharPos{col=10, row=10}}),
                                   ("rust", Just WordPos{start=CharPos{col=9, row=5}, end=CharPos{col=9, row=2}}),
                                   ("java", Just WordPos{start=CharPos{col=1, row=1}, end=CharPos{col=4, row=4}}),
                                   ("lua", Just WordPos{start=CharPos{col=8, row=9}, end=CharPos{col=6, row=7}})
                               ]
               }
        , Case { description = "Should locate words written bottom left to top right"
               , grid        = [
                                   "jefblpepre",
                                   "camdcimgtc",
                                   "oivokprjsm",
                                   "pbwasqroua",
                                   "rixilelhrs",
                                   "wolcqlirpc",
                                   "screeaumgr",
                                   "alxhpburyi",
                                   "jalaycalmp",
                                   "clojurermt" ]
               , wordList    = [ "clojure", "elixir", "ecmascript", "rust", "java", "lua", "lisp" ]
               , expected    = [
                                   ("clojure", Just WordPos{start=CharPos{col=1, row=10}, end=CharPos{col=7, row=10}}),
                                   ("elixir", Just WordPos{start=CharPos{col=6, row=5}, end=CharPos{col=1, row=5}}),
                                   ("ecmascript", Just WordPos{start=CharPos{col=10, row=1}, end=CharPos{col=10, row=10}}),
                                   ("rust", Just WordPos{start=CharPos{col=9, row=5}, end=CharPos{col=9, row=2}}),
                                   ("java", Just WordPos{start=CharPos{col=1, row=1}, end=CharPos{col=4, row=4}}),
                                   ("lua", Just WordPos{start=CharPos{col=8, row=9}, end=CharPos{col=6, row=7}}),
                                   ("lisp", Just WordPos{start=CharPos{col=3, row=6}, end=CharPos{col=6, row=3}})
                               ]
               }
        , Case { description = "Should locate words written top right to bottom left"
               , grid        = [
                                   "jefblpepre",
                                   "camdcimgtc",
                                   "oivokprjsm",
                                   "pbwasqroua",
                                   "rixilelhrs",
                                   "wolcqlirpc",
                                   "screeaumgr",
                                   "alxhpburyi",
                                   "jalaycalmp",
                                   "clojurermt"
                               ]
               , wordList    = [ "clojure", "elixir", "ecmascript", "rust", "java", "lua", "lisp", "ruby" ]
               , expected    = [
                                   ("clojure", Just WordPos{start=CharPos{col=1, row=10}, end=CharPos{col=7, row=10}}),
                                   ("elixir", Just WordPos{start=CharPos{col=6, row=5}, end=CharPos{col=1, row=5}}),
                                   ("ecmascript", Just WordPos{start=CharPos{col=10, row=1}, end=CharPos{col=10, row=10}}),
                                   ("rust", Just WordPos{start=CharPos{col=9, row=5}, end=CharPos{col=9, row=2}}),
                                   ("java", Just WordPos{start=CharPos{col=1, row=1}, end=CharPos{col=4, row=4}}),
                                   ("lua", Just WordPos{start=CharPos{col=8, row=9}, end=CharPos{col=6, row=7}}),
                                   ("lisp", Just WordPos{start=CharPos{col=3, row=6}, end=CharPos{col=6, row=3}}),
                                   ("ruby", Just WordPos{start=CharPos{col=8, row=6}, end=CharPos{col=5, row=9}})
                               ]
               }
        , Case { description = "Should fail to locate a word that is not in the puzzle"
               , grid        = [
                                   "jefblpepre",
                                   "camdcimgtc",
                                   "oivokprjsm",
                                   "pbwasqroua",
                                   "rixilelhrs",
                                   "wolcqlirpc",
                                   "screeaumgr",
                                   "alxhpburyi",
                                   "jalaycalmp",
                                   "clojurermt"
                               ]
               , wordList    = [ "clojure", "elixir", "ecmascript", "rust", "java", "lua", "lisp", "ruby", "haskell" ]
               , expected    = [
                                   ("clojure", Just WordPos{start=CharPos{col=1, row=10}, end=CharPos{col=7, row=10}}),
                                   ("elixir", Just WordPos{start=CharPos{col=6, row=5}, end=CharPos{col=1, row=5}}),
                                   ("ecmascript", Just WordPos{start=CharPos{col=10, row=1}, end=CharPos{col=10, row=10}}),
                                   ("rust", Just WordPos{start=CharPos{col=9, row=5}, end=CharPos{col=9, row=2}}),
                                   ("java", Just WordPos{start=CharPos{col=1, row=1}, end=CharPos{col=4, row=4}}),
                                   ("lua", Just WordPos{start=CharPos{col=8, row=9}, end=CharPos{col=6, row=7}}),
                                   ("lisp", Just WordPos{start=CharPos{col=3, row=6}, end=CharPos{col=6, row=3}}),
                                   ("ruby", Just WordPos{start=CharPos{col=8, row=6}, end=CharPos{col=5, row=9}}),
                                   ("haskell", Nothing)
                               ]
               }
        , Case { description = "Should fail to locate words that are not on horizontal, vertical, or diagonal lines"
               , grid        = [ "abc", "def" ]
               , wordList    = [ "aef", "ced", "abf", "cbd" ]
               , expected    = [
                                   ("aef", Nothing),
                                   ("ced", Nothing),
                                   ("abf", Nothing),
                                   ("cbd", Nothing)
                               ]
               }
        , Case { description = "Should not concatenate different lines to find a horizontal word"
               , grid        = [ "abceli", "xirdfg" ]
               , wordList    = [ "elixir" ]
               , expected    = [ ("elixir", Nothing) ]
               }
        , Case { description = "Should not wrap around horizontally to find a word"
               , grid        = [ "silabcdefp" ]
               , wordList    = [ "lisp" ]
               , expected    = [ ("lisp", Nothing) ]
               }
        , Case { description = "Should not wrap around vertically to find a word"
               , grid        = [ "s", "u", "r", "a", "b", "c", "t" ]
               , wordList    = [ "rust" ]
               , expected    = [ ("rust", Nothing) ]
               }
        ]
