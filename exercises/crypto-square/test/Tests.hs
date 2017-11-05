{-# LANGUAGE RecordWildCards #-}

import Data.Char         (isSpace)
import Data.Foldable     (for_)
import Data.Function     (on)
import Test.Hspec        (Spec, describe, it, shouldBe, shouldMatchList)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import CryptoSquare (encode)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "encode" $ for_ cases test
  where

    test Case{..} = describe description $ do

      let shouldMatchWords  = shouldBe        `on` words
          shouldMatchString = shouldBe        `on` filter (not . isSpace)
          shouldMatchChars  = shouldMatchList `on` filter (not . isSpace)

      it "normalizes the input"    $ encode input `shouldMatchChars`  expected
      it "reorders the characters" $ encode input `shouldMatchString` expected
      it "groups the output"       $ encode input `shouldMatchWords`  expected

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: String
                 }

cases :: [Case]
cases = [ Case { description = "empty plaintext results in an empty ciphertext"
               , input       = ""
               , expected    = ""
               }
        , Case { description = "Lowercase"
               , input       = "A"
               , expected    = "a"
               }
        , Case { description = "Remove spaces"
               , input       = "  b "
               , expected    = "b"
               }
        , Case { description = "Remove punctuation"
               , input       = "@1,%!"
               , expected    = "1"
               }
        , Case { description = "9 character plaintext results in 3 chunks of 3 characters"
               , input       = "This is fun!"
               , expected    = "tsf hiu isn"
               }
        , Case { description = "8 character plaintext results in 3 chunks, the last one with a trailing space"
               , input       = "Chill out."
               , expected    = "clu hlt io "
               }
        , Case { description = "54 character plaintext results in 7 chunks, the last two padded with spaces"
               , input       = "If man was meant to stay on the ground, god would have given us roots."
               , expected    = "imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn  sseoau "
               }
        ]
