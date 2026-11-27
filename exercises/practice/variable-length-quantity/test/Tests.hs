{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad
import qualified Data.Bits as Bits
import Data.Coerce
import Data.List
import qualified Data.Text as T
import Data.Word
import GHC.Exts (IsList)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)
import Test.QuickCheck
import Text.Printf
import VariableLengthQuantity

-- | Show instance that pretty-prints list of ints as 0-padded hex literal.
newtype PrettyHex a = PrettyHex [a] deriving (IsList, Eq, Foldable)

instance (PrintfArg i, Bits.FiniteBits i) => Show (PrettyHex i) where
  show (PrettyHex xs) = "[" <> intercalate "," (fmap ppr xs) <> "]"
    where
      ppr x = printf "0x%0*X" (Bits.finiteBitSize x `div` 4) x

data EncDec = Enc | Dec

type family CaseInputExpect (ed :: EncDec) where
  CaseInputExpect 'Enc = (PrettyHex Word32, PrettyHex Word8)
  CaseInputExpect 'Dec = (PrettyHex Word8, Either DecodeError (PrettyHex Word32))

data Case ty = Case
  { uuid :: T.Text
  , description :: T.Text
  , inputAndExpected :: CaseInputExpect ty
  }

deriving instance Show (CaseInputExpect ty) => Show (Case ty)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
  let -- coerce functions to be tests to use PrettyHex as Show instance.
      encodes' :: PrettyHex Word32 -> PrettyHex Word8
      encodes' = coerce encodes
      decodes' :: PrettyHex Word8 -> Either DecodeError (PrettyHex Word32)
      decodes' = coerce decodes
      mkCases testCases testFunc =
        forM_ testCases $
          \( Case
               { description
               , inputAndExpected = (input, expected)
               }
             ) -> do
              specify (T.unpack description) $
                testFunc input `shouldBe` expected
  describe "encodes" $ do
    -- serves as basic sanity checks
    prop "encoded result is non-empty" $
      \x -> not (null (encodes' [x]))
    prop "top bits are set correctly" $
      \x ->
        let PrettyHex encoded = encodes' [x]
            mask = 0b1000_0000
         in all ((/= 0) . (Bits..&. mask)) (init encoded)
              .&&. ((last encoded Bits..&. mask) == 0)
    mkCases encodeTests encodes'

  describe "decodes" $ do
    mkCases decodeTests decodes'
    -- cannonical data doesn't call for this type of exceptions so it gets tested last.
    prop "octet length of 5 but with too many bits" $ do
      lo <- choose (0, 0b0111_1111)
      mids <- replicateM 3 $ choose (0b1000_0000, 0b1111_1111)
      hi <- choose (0b1001_1111, 0b1111_1111)
      pure $ decodes' (PrettyHex ([hi] <> mids <> [lo])) === Left TooManyBits

  describe "properties" $
    -- those passing all canonical data shall face the ultimate lord of randomness.
    prop "decodes . encodes ~ id" $
      forAll
        (PrettyHex <$> listOf (choose (minBound, maxBound)))
        (\xs -> (decodes' . encodes') xs === Right xs)

encodeTests :: [Case 'Enc]
encodeTests =
  [ Case
      { uuid = "35c9db2e-f781-4c52-b73b-8e76427defd0"
      , description = "zero"
      , inputAndExpected = ([0x0000_0000], [0x00])
      }
  , Case
      { uuid = "be44d299-a151-4604-a10e-d4b867f41540"
      , description = "arbitrary single byte"
      , inputAndExpected = ([0x0000_0040], [0x40])
      }
  , Case
      { uuid = "890bc344-cb80-45af-b316-6806a6971e81"
      , description = "asymmetric single byte"
      , inputAndExpected = ([0x0000_0053], [0x53])
      }
  , Case
      { uuid = "ea399615-d274-4af6-bbef-a1c23c9e1346"
      , description = "largest single byte"
      , inputAndExpected = ([0x0000_007F], [0x7F])
      }
  , Case
      { uuid = "77b07086-bd3f-4882-8476-8dcafee79b1c"
      , description = "smallest double byte"
      , inputAndExpected = ([0x0000_0080], [0x81, 0x00])
      }
  , Case
      { uuid = "63955a49-2690-4e22-a556-0040648d6b2d"
      , description = "arbitrary double byte"
      , inputAndExpected = ([0x0000_2000], [0xC0, 0x00])
      }
  , Case
      { uuid = "4977d113-251b-4d10-a3ad-2f5a7756bb58"
      , description = "asymmetric double byte"
      , inputAndExpected = ([0x0000_00AD], [0x81, 0x2D])
      }
  , Case
      { uuid = "29da7031-0067-43d3-83a7-4f14b29ed97a"
      , description = "largest double byte"
      , inputAndExpected = ([0x0000_3FFF], [0xFF, 0x7F])
      }
  , Case
      { uuid = "3345d2e3-79a9-4999-869e-d4856e3a8e01"
      , description = "smallest triple byte"
      , inputAndExpected = ([0x0000_4000], [0x81, 0x80, 0x00])
      }
  , Case
      { uuid = "5df0bc2d-2a57-4300-a653-a75ee4bd0bee"
      , description = "arbitrary triple byte"
      , inputAndExpected = ([0x0010_0000], [0xC0, 0x80, 0x00])
      }
  , Case
      { uuid = "6731045f-1e00-4192-b5ae-98b22e17e9f7"
      , description = "asymmetric triple byte"
      , inputAndExpected = ([0x0001_D59C], [0x87, 0xAB, 0x1C])
      }
  , Case
      { uuid = "f51d8539-312d-4db1-945c-250222c6aa22"
      , description = "largest triple byte"
      , inputAndExpected = ([0x001F_FFFF], [0xFF, 0xFF, 0x7F])
      }
  , Case
      { uuid = "da78228b-544f-47b7-8bfe-d16b35bbe570"
      , description = "smallest quadruple byte"
      , inputAndExpected = ([0x0020_0000], [0x81, 0x80, 0x80, 0x00])
      }
  , Case
      { uuid = "11ed3469-a933-46f1-996f-2231e05d7bb6"
      , description = "arbitrary quadruple byte"
      , inputAndExpected = ([0x0800_0000], [0xC0, 0x80, 0x80, 0x00])
      }
  , Case
      { uuid = "b45ef770-cbba-48c2-bd3c-c6362679516e"
      , description = "asymmetric quadruple byte"
      , inputAndExpected = ([0x0035_7704], [0x81, 0xD5, 0xEE, 0x04])
      }
  , Case
      { uuid = "d5f3f3c3-e0f1-4e7f-aad0-18a44f223d1c"
      , description = "largest quadruple byte"
      , inputAndExpected = ([0x0FFF_FFFF], [0xFF, 0xFF, 0xFF, 0x7F])
      }
  , Case
      { uuid = "91a18b33-24e7-4bfb-bbca-eca78ff4fc47"
      , description = "smallest quintuple byte"
      , inputAndExpected = ([0x1000_0000], [0x81, 0x80, 0x80, 0x80, 0x00])
      }
  , Case
      { uuid = "5f34ff12-2952-4669-95fe-2d11b693d331"
      , description = "arbitrary quintuple byte"
      , inputAndExpected = ([0xFF00_0000], [0x8F, 0xF8, 0x80, 0x80, 0x00])
      }
  , Case
      { uuid = "9be46731-7cd5-415c-b960-48061cbc1154"
      , description = "asymmetric quintuple byte"
      , inputAndExpected = ([0x8665_6105], [0x88, 0xB3, 0x95, 0xC2, 0x05])
      }
  , Case
      { uuid = "7489694b-88c3-4078-9864-6fe802411009"
      , description = "maximum 32-bit integer input"
      , inputAndExpected = ([0xFFFF_FFFF], [0x8F, 0xFF, 0xFF, 0xFF, 0x7F])
      }
  , Case
      { uuid = "f9b91821-cada-4a73-9421-3c81d6ff3661"
      , description = "two single-byte values"
      , inputAndExpected = ([0x0000_0040, 0x0000_007F], [0x40, 0x7F])
      }
  , Case
      { uuid = "68694449-25d2-4974-ba75-fa7bb36db212"
      , description = "two multi-byte values"
      , inputAndExpected = ([0x0000_4000, 0x0012_3456], [0x81, 0x80, 0x00, 0xC8, 0xE8, 0x56])
      }
  , Case
      { uuid = "51a06b5c-de1b-4487-9a50-9db1b8930d85"
      , description = "many multi-byte values"
      , inputAndExpected =
          ( [0x0000_2000, 0x0012_3456, 0x0FFF_FFFF, 0x0000_0000, 0x0000_3FFF, 0x0000_4000]
          , [0xC0, 0x00, 0xC8, 0xE8, 0x56, 0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0xFF, 0x7F, 0x81, 0x80, 0x00]
          )
      }
  ]

decodeTests :: [Case 'Dec]
decodeTests =
  [ Case
      { uuid = "baa73993-4514-4915-bac0-f7f585e0e59a"
      , description = "one byte"
      , inputAndExpected = ([0x7F], Right [0x0000_007F])
      }
  , Case
      { uuid = "72e94369-29f9-46f2-8c95-6c5b7a595aee"
      , description = "two bytes"
      , inputAndExpected = ([0xC0, 0x00], Right [0x0000_2000])
      }
  , Case
      { uuid = "df5a44c4-56f7-464e-a997-1db5f63ce691"
      , description = "three bytes"
      , inputAndExpected = ([0xFF, 0xFF, 0x7F], Right [0x001F_FFFF])
      }
  , Case
      { uuid = "1bb58684-f2dc-450a-8406-1f3452aa1947"
      , description = "four bytes"
      , inputAndExpected = ([0x81, 0x80, 0x80, 0x00], Right [0x0020_0000])
      }
  , Case
      { uuid = "cecd5233-49f1-4dd1-a41a-9840a40f09cd"
      , description = "maximum 32-bit integer"
      , inputAndExpected = ([0x8F, 0xFF, 0xFF, 0xFF, 0x7F], Right [0xFFFF_FFFF])
      }
  , Case
      { uuid = "e7d74ba3-8b8e-4bcb-858d-d08302e15695"
      , description = "incomplete sequence causes error"
      , inputAndExpected = ([0xFF], Left IncompleteSequence)
      }
  , Case
      { uuid = "aa378291-9043-4724-bc53-aca1b4a3fcb6"
      , description = "incomplete sequence causes error, even if value is zero"
      , inputAndExpected = ([0x80], Left IncompleteSequence)
      }
  , Case
      { uuid = "a91e6f5a-c64a-48e3-8a75-ce1a81e0ebee"
      , description = "multiple values"
      , inputAndExpected =
          ( [0xC0, 0x00, 0xC8, 0xE8, 0x56, 0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0xFF, 0x7F, 0x81, 0x80, 0x00]
          , Right [0x0000_2000, 0x0012_3456, 0x0FFF_FFFF, 0x0000_0000, 0x0000_3FFF, 0x0000_4000]
          )
      }
  ]
