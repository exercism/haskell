{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import RailFenceCipher (encode, decode)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
          describe "encode" $ for_ encodeCases testE
          describe "decode" $ for_ decodeCases testD
  where
    testE Case{..} = it description $ encode key text `shouldBe` expected
    testD Case{..} = it description $ decode key text `shouldBe` expected

data Case = Case { description :: String
                 , key         :: Int
                 , text        :: String
                 , expected    :: String
                 }

encodeCases :: [Case]
encodeCases = [ Case { description = "encode with two rails"
                     , key         = 2
                     , text        = "XOXOXOXOXOXOXOXOXO"
                     , expected    = "XXXXXXXXXOOOOOOOOO"
                     }
              , Case { description = "encode with three rails"
                     , key         = 3
                     , text        = "WEAREDISCOVEREDFLEEATONCE"
                     , expected    = "WECRLTEERDSOEEFEAOCAIVDEN"
                     }
              , Case { description = "encode with ending in the middle"
                     , key         = 4
                     , text        = "EXERCISES"
                     , expected    = "ESXIEECSR"
                     }
              ]

decodeCases :: [Case]
decodeCases = [ Case { description = "decode with three rails"
                     , key         = 3
                     , text        = "TEITELHDVLSNHDTISEIIEA"
                     , expected    = "THEDEVILISINTHEDETAILS"
                     }
              , Case { description = "decode with five rails"
                     , key         = 5
                     , text        = "EIEXMSMESAORIWSCE"
                     , expected    = "EXERCISMISAWESOME"
                     }
              , Case { description = "decode with six rails"
                     , key         = 6
                     , text        = "133714114238148966225439541018335470986172518171757571896261"
                     , expected    = "112358132134558914423337761098715972584418167651094617711286"
                     }
              ]
