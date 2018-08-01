{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Atbash (encode, decode)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
          describe "encode" $ for_ encodeCases $ test encode
          describe "decode" $ for_ decodeCases $ test decode
  where
    test f Case{..} = it description $ f phrase `shouldBe` expected

data Case = Case { description :: String
                 , phrase      :: String
                 , expected    :: String
                 }

encodeCases :: [Case]
encodeCases =
    [ Case { description = "encode yes"
           , phrase      = "yes"
           , expected    = "bvh"
           }
    , Case { description = "encode no"
           , phrase      = "no"
           , expected    = "ml"
           }
    , Case { description = "encode OMG"
           , phrase      = "OMG"
           , expected    = "lnt"
           }
    , Case { description = "encode spaces"
           , phrase      = "O M G"
           , expected    = "lnt"
           }
    , Case { description = "encode mindblowingly"
           , phrase      = "mindblowingly"
           , expected    = "nrmwy oldrm tob"
           }
    , Case { description = "encode numbers"
           , phrase      = "Testing,1 2 3, testing."
           , expected    = "gvhgr mt123 gvhgr mt"
           }
    , Case { description = "encode deep thought"
           , phrase      = "Truth is fiction."
           , expected    = "gifgs rhurx grlm"
           }
    , Case { description = "encode all the letters"
           , phrase      = "The quick brown fox jumps over the lazy dog."
           , expected    = "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt"
           }
    ]

decodeCases :: [Case]
decodeCases =
    [ Case { description = "decode exercism"
           , phrase      = "vcvix rhn"
           , expected    = "exercism"
           }
    , Case { description = "decode a sentence"
           , phrase      = "zmlyh gzxov rhlug vmzhg vkkrm thglm v"
           , expected    = "anobstacleisoftenasteppingstone"
           }
    , Case { description = "decode numbers"
           , phrase      = "gvhgr mt123 gvhgr mt"
           , expected    = "testing123testing"
           }
    , Case { description = "decode all the letters"
           , phrase      = "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt"
           , expected    = "thequickbrownfoxjumpsoverthelazydog"
           }
    , Case { description = "decode with too many spaces"
           , phrase      = "vc vix    r hn"
           , expected    = "exercism"
           }
    , Case { description = "decode with no spaces"
           , phrase      = "zmlyhgzxovrhlugvmzhgvkkrmthglmv"
           , expected    = "anobstacleisoftenasteppingstone"
           }
    ]
