module Vlq
  ( encodes
  , decodes
  , DecodeError (..)
  )
where

import Data.Word

data DecodeError
  = IncompleteSequence
  | TooManyBits
  deriving (Show, Eq)

encodes :: [Word32] -> [Word8]
encodes = error "You need to implement this function."

decodes :: [Word8] -> Either DecodeError [Word32]
decodes = error "You need to implement this function."
