module RationalNumbers
(Rational,
 abs,
 numerator,
 denominator,
 add,
 sub,
 mul,
 div,
 pow,
 expRational,
 expReal,
 rational) where

import Prelude hiding (div, abs, Rational)
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Rational a = Rational a a deriving(Eq, Show)

rational :: Integral a => (a, a) -> Rational a
rational (n, d) = Rational (n' `quot` g) (d' `quot` g)
    where
        g = gcd n d
        (n', d') = if d < 0 then (-n, -d) else (n, d)

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational n d) = rational (P.abs n, P.abs d)

numerator :: Integral a => Rational a -> a
numerator (Rational n _) = n

denominator :: Integral a => Rational a -> a
denominator (Rational _ d) = d

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational n1 d1) (Rational n2 d2) = let dd = d1*d2 in rational (n1*d2 + n2*d1, dd)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational n1 d1) (Rational n2 d2) = let dd = d1*d2 in rational (n1*d2 - n2*d1, dd)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational n1 d1) (Rational n2 d2) = rational (n1*n2, d1*d2)

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rational n1 d1) (Rational n2 d2) = rational (n1*d2, d1*n2)

pow :: Integral a => Rational a -> a -> Rational a
pow (Rational n d) num = if num >= 0 then rational (n^num, d^num) else rational (d^(-num), n^(-num))

expRational :: Integral a => Floating b => Rational a -> b -> b
expRational (Rational n d) num = (fromIntegral n / fromIntegral d) ** num

expReal :: Floating a => Integral b => a -> Rational b -> a
expReal num (Rational n d) = num ** (fromIntegral n / fromIntegral d)
