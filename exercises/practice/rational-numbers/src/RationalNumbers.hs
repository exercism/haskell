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

-- Data definition -------------------------------------------------------------
data Rational a = Dummy deriving(Eq, Show)

rational :: Integral a => (a, a) -> Rational a
rational = error "You need to implement this function"

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs = error "You need to implement this function"

numerator :: Integral a => Rational a -> a
numerator = error "You need to implement this function"

denominator :: Integral a => Rational a -> a
denominator = error "You need to implement this function"

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add = error "You need to implement this function"

sub :: Integral a => Rational a -> Rational a -> Rational a
sub = error "You need to implement this function"

mul :: Integral a => Rational a -> Rational a -> Rational a
mul = error "You need to implement this function"

div :: Integral a => Rational a -> Rational a -> Rational a
div = error "You need to implement this function"

pow :: Integral a => Rational a -> a -> Rational a
pow = error "You need to implement this function"

expRational :: Integral a => Floating b => Rational a -> b -> b
expRational = error "You need to implement this function"

expReal :: Floating a => Integral b => a -> Rational b -> a
expReal = error "You need to implement this function"
