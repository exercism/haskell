module ComplexNumbers
(Complex,
 conjugate,
 abs,
 exp,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs, exp)

-- Data definition -------------------------------------------------------------
data Complex a = Dummy deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex = error "You need to implement this function"

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate = error "You need to implement this function"

abs :: Floating a => Complex a -> a
abs = error "You need to implement this function"

real :: Num a => Complex a -> a
real = error "You need to implement this function"

imaginary :: Num a => Complex a -> a
imaginary = error "You need to implement this function"

exp :: Floating a => Complex a -> Complex a
exp = error "You need to implement this function"

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul = error "You need to implement this function"

add :: Num a => Complex a -> Complex a -> Complex a
add = error "You need to implement this function"

sub :: Num a => Complex a -> Complex a -> Complex a
sub = error "You need to implement this function"

div :: Fractional a => Complex a -> Complex a -> Complex a
div = error "You need to implement this function"
