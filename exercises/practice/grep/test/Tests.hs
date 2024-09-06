{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Grep (grep, Flag(..))

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "grep" $ for_ cases test
  where
    test Case{..} = it description $ grep pattern flags files `shouldBe` expected

data Case = Case { description :: String
                 , pattern     :: String
                 , flags       :: [Flag]
                 , files       :: [(String, [String])]
                 , expected    :: [String]
                 }

cases :: [Case]
cases =
    [ Case { description = "One file, one match, no flags"
           , pattern     = "Agamemnon"
           , flags       = []
           , files       = [iliad]
           , expected    = ["Of Atreus, Agamemnon, King of men."]
           }
    , Case { description = "One file, one match, print line numbers flag"
           , pattern     = "Forbidden"
           , flags       = [N]
           , files       = [paradiseLost]
           , expected    = ["2:Of that Forbidden Tree, whose mortal tast"]
           }
    , Case { description = "One file, one match, case-insensitive flag"
           , pattern     = "FORBIDDEN"
           , flags       = [I]
           , files       = [paradiseLost]
           , expected    = ["Of that Forbidden Tree, whose mortal tast"]
           }
    , Case { description = "One file, one match, print file names flag"
           , pattern     = "Forbidden"
           , flags       = [L]
           , files       = [paradiseLost]
           , expected    = ["paradise-lost.txt"]
           }
    , Case { description = "One file, one match, match entire lines flag"
           , pattern     = "With loss of Eden, till one greater Man"
           , flags       = [X]
           , files       = [paradiseLost]
           , expected    = ["With loss of Eden, till one greater Man"]
           }
    , Case { description = "One file, one match, multiple flags"
           , pattern     = "OF ATREUS, Agamemnon, KIng of MEN."
           , flags       = [N, I, X]
           , files       = [iliad]
           , expected    = ["9:Of Atreus, Agamemnon, King of men."]
           }
    , Case { description = "One file, several matches, no flags"
           , pattern     = "may"
           , flags       = []
           , files       = [midsummerNight]
           , expected    = [
                            "Nor how it may concern my modesty,",
                            "But I beseech your grace that I may know",
                            "The worst that may befall me in this case,"
                           ]
           }
    , Case { description = "One file, several matches, print line numbers flag"
           , pattern     = "may"
           , flags       = [N]
           , files       = [midsummerNight]
           , expected    = [
                            "3:Nor how it may concern my modesty,",
                            "5:But I beseech your grace that I may know",
                            "6:The worst that may befall me in this case,"
                           ]
           }
    , Case { description = "One file, several matches, match entire lines flag"
           , pattern     = "may"
           , flags       = [X]
           , files       = [midsummerNight]
           , expected    = []
           }
    , Case { description = "One file, several matches, case-insensitive flag"
           , pattern     = "ACHILLES"
           , flags       = [I]
           , files       = [iliad]
           , expected    = [
                            "Achilles sing, O Goddess! Peleus' son;",
                            "The noble Chief Achilles from the son"
                           ]
           }
    , Case { description = "One file, several matches, inverted flag"
           , pattern     = "Of"
           , flags       = [V]
           , files       = [paradiseLost]
           , expected    = [
                            "Brought Death into the World, and all our woe,",
                            "With loss of Eden, till one greater Man",
                            "Restore us, and regain the blissful Seat,",
                            "Sing Heav'nly Muse, that on the secret top",
                            "That Shepherd, who first taught the chosen Seed"
                           ]
           }
    , Case { description = "One file, no matches, various flags"
           , pattern     = "Gandalf"
           , flags       = [N, L, X, I]
           , files       = [iliad]
           , expected    = []
           }
    , Case { description = "One file, one match, file flag takes precedence over line flag"
           , pattern     = "ten"
           , flags       = [N, L]
           , files       = [iliad]
           , expected    = ["iliad.txt"]
           }
    , Case { description = "One file, several matches, inverted and match entire lines flags"
           , pattern     = "Illustrious into Ades premature,"
           , flags       = [X, V]
           , files       = [iliad]
           , expected    = [
                            "Achilles sing, O Goddess! Peleus' son;",
                            "His wrath pernicious, who ten thousand woes",
                            "Caused to Achaia's host, sent many a soul",
                            "And Heroes gave (so stood the will of Jove)",
                            "To dogs and to all ravening fowls a prey,",
                            "When fierce dispute had separated once",
                            "The noble Chief Achilles from the son",
                            "Of Atreus, Agamemnon, King of men."
                           ]
           }
    , Case { description = "Multiple files, one match, no flags"
           , pattern     = "Agamemnon"
           , flags       = []
           , files       = [iliad, midsummerNight, paradiseLost]
           , expected    = ["iliad.txt:Of Atreus, Agamemnon, King of men."]
           }
    , Case { description = "Multiple files, several matches, no flags"
           , pattern     = "may"
           , flags       = []
           , files       = [iliad, midsummerNight, paradiseLost]
           , expected    = [
                            "midsummer-night.txt:Nor how it may concern my modesty,",
                            "midsummer-night.txt:But I beseech your grace that I may know",
                            "midsummer-night.txt:The worst that may befall me in this case,"
                           ]
           }
    , Case { description = "Multiple files, several matches, print line numbers flag"
           , pattern     = "that"
           , flags       = [N]
           , files       = [iliad, midsummerNight, paradiseLost]
           , expected    = [
                            "midsummer-night.txt:5:But I beseech your grace that I may know",
                            "midsummer-night.txt:6:The worst that may befall me in this case,",
                            "paradise-lost.txt:2:Of that Forbidden Tree, whose mortal tast",
                            "paradise-lost.txt:6:Sing Heav'nly Muse, that on the secret top"
                           ]
           }
    , Case { description = "Multiple files, one match, print file names flag"
           , pattern     = "who"
           , flags       = [L]
           , files       = [iliad, midsummerNight, paradiseLost]
           , expected    = ["iliad.txt", "paradise-lost.txt"]
           }
    , Case { description = "Multiple files, several matches, case-insensitive flag"
           , pattern     = "TO"
           , flags       = [I]
           , files       = [iliad, midsummerNight, paradiseLost]
           , expected    = [
                            "iliad.txt:Caused to Achaia's host, sent many a soul",
                            "iliad.txt:Illustrious into Ades premature,",
                            "iliad.txt:And Heroes gave (so stood the will of Jove)",
                            "iliad.txt:To dogs and to all ravening fowls a prey,",
                            "midsummer-night.txt:I do entreat your grace to pardon me.",
                            "midsummer-night.txt:In such a presence here to plead my thoughts;",
                            "midsummer-night.txt:If I refuse to wed Demetrius.",
                            "paradise-lost.txt:Brought Death into the World, and all our woe,",
                            "paradise-lost.txt:Restore us, and regain the blissful Seat,",
                            "paradise-lost.txt:Sing Heav'nly Muse, that on the secret top"
                           ]
           }
    , Case { description = "Multiple files, several matches, inverted flag"
           , pattern     = "a"
           , flags       = [V]
           , files       = [iliad, midsummerNight, paradiseLost]
           , expected    = [
                            "iliad.txt:Achilles sing, O Goddess! Peleus' son;",
                            "iliad.txt:The noble Chief Achilles from the son",
                            "midsummer-night.txt:If I refuse to wed Demetrius."
                           ]
           }
    , Case { description = "Multiple files, one match, match entire lines flag"
           , pattern     = "But I beseech your grace that I may know"
           , flags       = [X]
           , files       = [iliad, midsummerNight, paradiseLost]
           , expected    = ["midsummer-night.txt:But I beseech your grace that I may know"]
           }
    , Case { description = "Multiple files, one match, multiple flags"
           , pattern     = "WITH LOSS OF EDEN, TILL ONE GREATER MAN"
           , flags       = [N, I, X]
           , files       = [iliad, midsummerNight, paradiseLost]
           , expected    = ["paradise-lost.txt:4:With loss of Eden, till one greater Man"]
           }
    , Case { description = "Multiple files, no matches, various flags"
           , pattern     = "Frodo"
           , flags       = [N, L, X, I]
           , files       = [iliad, midsummerNight, paradiseLost]
           , expected    = []
           }
    , Case { description = "Multiple files, several matches, file flag takes precedence over line number flag"
           , pattern     = "who"
           , flags       = [N, L]
           , files       = [iliad, midsummerNight, paradiseLost]
           , expected    = ["iliad.txt", "paradise-lost.txt"]
           }
    , Case { description = "Multiple files, several matches, inverted and match entire lines flags"
           , pattern     = "Illustrious into Ades premature,"
           , flags       = [X, V]
           , files       = [iliad, midsummerNight, paradiseLost]
           , expected    = [
                            "iliad.txt:Achilles sing, O Goddess! Peleus' son;",
                            "iliad.txt:His wrath pernicious, who ten thousand woes",
                            "iliad.txt:Caused to Achaia's host, sent many a soul",
                            "iliad.txt:And Heroes gave (so stood the will of Jove)",
                            "iliad.txt:To dogs and to all ravening fowls a prey,",
                            "iliad.txt:When fierce dispute had separated once",
                            "iliad.txt:The noble Chief Achilles from the son",
                            "iliad.txt:Of Atreus, Agamemnon, King of men.",
                            "midsummer-night.txt:I do entreat your grace to pardon me.",
                            "midsummer-night.txt:I know not by what power I am made bold,",
                            "midsummer-night.txt:Nor how it may concern my modesty,",
                            "midsummer-night.txt:In such a presence here to plead my thoughts;",
                            "midsummer-night.txt:But I beseech your grace that I may know",
                            "midsummer-night.txt:The worst that may befall me in this case,",
                            "midsummer-night.txt:If I refuse to wed Demetrius.",
                            "paradise-lost.txt:Of Mans First Disobedience, and the Fruit",
                            "paradise-lost.txt:Of that Forbidden Tree, whose mortal tast",
                            "paradise-lost.txt:Brought Death into the World, and all our woe,",
                            "paradise-lost.txt:With loss of Eden, till one greater Man",
                            "paradise-lost.txt:Restore us, and regain the blissful Seat,",
                            "paradise-lost.txt:Sing Heav'nly Muse, that on the secret top",
                            "paradise-lost.txt:Of Oreb, or of Sinai, didst inspire",
                            "paradise-lost.txt:That Shepherd, who first taught the chosen Seed"
                           ]
           }
    ]

iliad =
       ("iliad.txt", [
              "Achilles sing, O Goddess! Peleus' son;",
              "His wrath pernicious, who ten thousand woes",
              "Caused to Achaia's host, sent many a soul",
              "Illustrious into Ades premature,",
              "And Heroes gave (so stood the will of Jove)",
              "To dogs and to all ravening fowls a prey,",
              "When fierce dispute had separated once",
              "The noble Chief Achilles from the son",
              "Of Atreus, Agamemnon, King of men."
       ])
midsummerNight =
       ("midsummer-night.txt", [
              "I do entreat your grace to pardon me.",
              "I know not by what power I am made bold,",
              "Nor how it may concern my modesty,",
              "In such a presence here to plead my thoughts;",
              "But I beseech your grace that I may know",
              "The worst that may befall me in this case,",
              "If I refuse to wed Demetrius."
       ])
paradiseLost =
       ("paradise-lost.txt", [
              "Of Mans First Disobedience, and the Fruit",
              "Of that Forbidden Tree, whose mortal tast",
              "Brought Death into the World, and all our woe,",
              "With loss of Eden, till one greater Man",
              "Restore us, and regain the blissful Seat,",
              "Sing Heav'nly Muse, that on the secret top",
              "Of Oreb, or of Sinai, didst inspire",
              "That Shepherd, who first taught the chosen Seed"
       ])