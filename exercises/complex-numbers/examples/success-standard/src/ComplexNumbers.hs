module ComplexNumbers
(Complex(..),
 conjugate,
 abs,
 mul,
 add,
 sub,
 div) where

import Prelude hiding (div, abs)

-- Data definition -------------------------------------------------------------
data Complex = Complex {real :: Float, imaginary :: Float} deriving Eq

instance Show (Complex) where
  show (Complex a b) = "("++(show a)++" + "++(show b)++"i)"
--------------------------------------------------------------------------------
exp2 :: Float -> Float
exp2 a = a ^ (2 :: Integer)

-- unary operators -------------------------------------------------------------
conjugate :: Complex -> Complex
conjugate (Complex a b) = Complex a (-b)

abs :: Complex -> Float
abs (Complex a b) = sqrt ((exp2 a) + (exp2 b))

-- binary operators ------------------------------------------------------------
mul :: Complex -> Complex -> Complex
mul (Complex a b) (Complex c d) = (Complex (a*c - b*d) (a*d + b*c))

add :: Complex -> Complex -> Complex
add (Complex a b) (Complex c d) = (Complex (a+c) (b+d))

sub :: Complex -> Complex -> Complex
sub (Complex a b) (Complex c d) = (Complex (a-c) (b-d))

div :: Complex -> Complex -> Complex
div (Complex a b) (Complex c d) = (Complex ((a*c + b*d)/((exp2 c) + (exp2 d)))
                                           ((b*c - a*d)/((exp2 c) + (exp2 d))))
