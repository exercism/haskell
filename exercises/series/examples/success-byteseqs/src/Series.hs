module Series (slices) where

import Control.Applicative   (liftA2)
import Control.Monad         (guard)
import Data.ByteString.Char8 (ByteString, foldr, length, tails, take)
import Data.Function         (on)
import Data.Maybe            (mapMaybe)
import Data.Sequence         (Seq, empty, fromList, (<|))
import Prelude        hiding (foldr, length, take)

slices :: Num a => Int -> ByteString -> Seq (Seq a)
slices n = fromList
         . mapMaybe toDigits
         . takeWhile ((== n) . length)
         . map (take n)
         . tails

toDigits :: Num a => ByteString -> Maybe (Seq a)
toDigits = foldr (liftA2 (<|) . toDigit) (Just empty)

toDigit :: Num a => Char -> Maybe a
toDigit x = guard (0 <= dec && dec <= 9) >> return (fromIntegral dec)
  where
    dec = ((-) `on` fromEnum) x '0'
