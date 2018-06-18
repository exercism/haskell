module ComplexNumbers
(Complex,
 conjugate,
 abs,
 exp,
 mul,
 add,
 sub,
 div,
 real,
 imaginary,
 complex) where

import Prelude hiding (div, abs, exp)
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Complex a = Complex a a deriving (Eq, Show)

--------------------------------------------------------------------------------
complex :: (a, a) -> Complex a
complex = uncurry Complex

exp2 :: Num a => a -> a
exp2 x = x^(2 :: Integer)

-- unary operators -------------------------------------------------------------
real :: Num a => Complex a -> a
real (Complex a _) = a

imaginary :: Num a => Complex a -> a
imaginary (Complex _ b) = b

conjugate :: Num a => Complex a -> Complex a
conjugate (Complex a b) = Complex a (-b)

abs :: Floating a => Complex a -> a
abs (Complex a b) = sqrt (exp2 a + exp2 b)

exp :: Num a => Complex a -> Complex a
exp (Complex a b) = Complex (cos b * P.exp a) (sin b * P.exp a)

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex a b) (Complex c d) = Complex (a*c - b*d) (a*d + b*c)

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex a b) (Complex c d) = Complex (a+c) (b+d)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex a b) (Complex c d) = Complex (a-c) (b-d)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex a b) (Complex c d) = Complex ((a*c + b*d)/(exp2 c + exp2 d))
                                          ((b*c - a*d)/(exp2 c + exp2 d))
