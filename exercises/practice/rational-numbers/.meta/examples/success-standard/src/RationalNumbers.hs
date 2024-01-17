module RationalNumbers
(Rational,
 abs,
 reduce,
 add,
 sub,
 mul,
 div,
 exprational,
 expreal,
 rational) where

import Prelude hiding (div, abs, Rational)
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Rational a = Rational a a deriving(Eq, Show)

rational :: (a, a) -> Rational a
rational (n, d) = Rational n d

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational n d ) = reduce $ Rational (P.abs n) (P.abs d)

reduce :: Integral a => Rational a -> Rational a
reduce (Rational n d) = Rational (n' `quot` g) (d' `quot` g)
    where
        g = gcd n d
        (n', d') = if d < 0 then (-n, -d) else (n, d)


-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational n1 d1) (Rational n2 d2) = let dd = d1*d2 in reduce $ Rational (n1*d2 + n2*d1) dd

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational n1 d1) (Rational n2 d2) = let dd = d1*d2 in reduce $ Rational (n1*d2 - n2*d1) dd

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational n1 d1) (Rational n2 d2) = reduce $ Rational (n1*n2) (d1*d2)

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rational n1 d1) (Rational n2 d2) = reduce $ Rational (n1*d2) (d1*n2)

exprational :: Integral a => Rational a -> a -> Rational a
exprational (Rational n d) num = reduce $ if num >= 0 then Rational (n^num) (d^num) else Rational (d^(-num)) (n^(-num))

expreal :: Floating a => Integral b => a -> Rational b -> a
expreal num (Rational n d) = num ** (fromIntegral n / fromIntegral d)
