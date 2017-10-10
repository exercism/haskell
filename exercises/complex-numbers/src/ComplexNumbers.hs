module ComplexNumbers
(Complex(..),
 conjugate,
 abs,
 real,
 imaginary,
 mul,
 add,
 sub,
 div) where

import Prelude hiding (div, abs)

-- Data definition -------------------------------------------------------------
data Complex = Dummy

-- unary operators -------------------------------------------------------------
conjugate :: Complex -> Complex
conjugate = error "You need to implement this function"

abs :: Complex -> Float
abs = error "You need to implement this function"

real :: Complex -> Float
real = error "You need to implement this function"

imaginary :: Complex -> Float
imaginary = error "You need to implement this function"

-- binary operators ------------------------------------------------------------
mul :: Complex -> Complex -> Complex
mul = error "You need to implement this function"

add :: Complex -> Complex -> Complex
add = error "You need to implement this function"

sub :: Complex -> Complex -> Complex
sub = error "You need to implement this function"

div :: Complex -> Complex -> Complex
div = error "You need to implement this function"
