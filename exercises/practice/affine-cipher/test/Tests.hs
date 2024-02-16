{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)
import Data.String       (fromString)

import Affine (encode, decode)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
          describe "encode" $ for_ encodeCases $ test encode
          describe "decode" $ for_ decodeCases $ test decode
  where
    test f Case{..} = it description $ f key (fromString phrase) `shouldBe` (fromString <$> expected)

data Case = Case { description :: String
                 , phrase      :: String
                 , key         :: (Int, Int)
                 , expected    :: Maybe String
                 }

encodeCases :: [Case]
encodeCases =
    [ Case { description = "encode yes"
           , phrase      = "yes"
           , key         = (5, 7)
           , expected    = Just "xbt"
           }
    , Case { description = "encode no"
           , phrase      = "no"
           , key         = (15, 18)
           , expected    = Just "fu"
           }
    , Case { description = "encode OMG"
           , phrase      = "OMG"
           , key         = (21, 3)
           , expected    = Just "lvz"
           }
    , Case { description = "encode spaces"
           , phrase      = "O M G"
           , key         = (25, 47)
           , expected    = Just "hjp"
           }
    , Case { description = "encode mindblowingly"
           , phrase      = "mindblowingly"
           , key         = (11, 15)
           , expected    = Just "rzcwa gnxzc dgt"
           }
    , Case { description = "encode numbers"
           , phrase      = "Testing,1 2 3, testing."
           , key         = (3, 4)
           , expected    = Just "jqgjc rw123 jqgjc rw"
           }
    , Case { description = "encode deep thought"
           , phrase      = "Truth is fiction."
           , key         = (5, 17)
           , expected    = Just "iynia fdqfb ifje"
           }
    , Case { description = "encode all the letters"
           , phrase      = "The quick brown fox jumps over the lazy dog."
           , key         = (17, 33)
           , expected    = Just "swxtj npvyk lruol iejdc blaxk swxmh qzglf"
           }
    , Case { description = "encode with a not coprime to m"
           , phrase      = "This is a test."
           , key         = (6, 17)
           , expected    = Nothing
           }
    ]

decodeCases :: [Case]
decodeCases =
    [ Case { description = "decode exercism"
           , phrase      = "tytgn fjr"
           , key         = (3, 7)
           , expected    = Just "exercism"
           }
    , Case { description = "decode a sentence"
           , phrase      = "qdwju nqcro muwhn odqun oppmd aunwd o"
           , key         = (19, 16)
           , expected    = Just "anobstacleisoftenasteppingstone"
           }
    , Case { description = "decode numbers"
           , phrase      = "odpoz ub123 odpoz ub"
           , key         = (25, 7)
           , expected    = Just "testing123testing"
           }
    , Case { description = "decode all the letters"
           , phrase      = "swxtj npvyk lruol iejdc blaxk swxmh qzglf"
           , key         = (17, 33)
           , expected    = Just "thequickbrownfoxjumpsoverthelazydog"
           }
    , Case { description = "decode with no spaces in input"
           , phrase      = "swxtjnpvyklruoliejdcblaxkswxmhqzglf"
           , key         = (17, 33)
           , expected    = Just "thequickbrownfoxjumpsoverthelazydog"
           }
    , Case { description = "decode with too many spaces"
           , phrase      = "vszzm    cly   yd cg    qdp"
           , key         = (15, 16)
           , expected    = Just "jollygreengiant"
           }
    , Case { description = "decode with a not coprime to m"
           , phrase      = "Test"
           , key         = (13, 5)
           , expected    = Nothing
           }
    ]
