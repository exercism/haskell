{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

module Vlq
  ( encodes
  , decodes
  , DecodeError (..)
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Bits
import Data.List
import Data.Word

data DecodeError
  = IncompleteSequence
  | TooManyBits
  deriving (Show, Eq)

encodeOne :: Word32 -> [Word8]
encodeOne 0 = [0]
encodeOne x = reverse . unfoldr go $ (x, True)
  where
    go (cur, fstOctet) = do
      guard $ cur /= 0
      let (q, r) = cur `quotRem` 0b1000_0000
          r' = fromIntegral $ if fstOctet then r else r .|. 0b1000_0000
      pure (r', (q, False))

encodes :: [Word32] -> [Word8]
encodes = concatMap encodeOne

decodeOne :: MonadError DecodeError m => [Word8] -> m Word32
decodeOne xs = do
  let l = length xs
  when (l == 0 || l > 5) $
    throwError IncompleteSequence
  when (l == 5 && head xs > 0b1000_1111) $
    throwError TooManyBits
  pure $ foldl (\acc x -> (acc `unsafeShiftL` 7) .|. (fromIntegral x .&. 0b0111_1111)) 0 xs

mayDecodeNext :: (MonadState [Word8] m, MonadError DecodeError m) => m (Maybe Word32)
mayDecodeNext =
  get >>= \case
    [] -> pure Nothing
    st
      | (highs, rest) <- span ((/= 0) . (.&. 0b1000_0000)) st ->
        Just
          <$> case rest of
            [] -> throwError IncompleteSequence
            low : rest' -> do
              put rest'
              decodeOne (highs <> [low])

decodes :: [Word8] -> Either DecodeError [Word32]
decodes =
  evalState
    (runExceptT $
       fix $ \next ->
         mayDecodeNext >>= \case
           Nothing -> pure []
           Just x -> (x :) <$> next)
