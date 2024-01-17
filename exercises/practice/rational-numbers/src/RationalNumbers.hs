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

-- Data definition -------------------------------------------------------------
data Rational a = Dummy deriving(Eq, Show)

rational :: (a, a) -> Rational a
rational = error "You need to implement this function"

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs = error "You need to implement this function"

reduce :: Integral a => Rational a -> Rational a
reduce = error "You need to implement this function"

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add = error "You need to implement this function"

sub :: Integral a => Rational a -> Rational a -> Rational a
sub = error "You need to implement this function"

mul :: Integral a => Rational a -> Rational a -> Rational a
mul = error "You need to implement this function"

div :: Integral a => Rational a -> Rational a -> Rational a
div = error "You need to implement this function"

exprational :: Integral a => Rational a -> a -> Rational a
exprational = error "You need to implement this function"

expreal :: Floating a => Integral b => a -> Rational b -> a
expreal = error "You need to implement this function"

