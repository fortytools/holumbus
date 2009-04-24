module MyExamples.Mandel.Mandel
(
    pixelAt
  , pixelAt'
)
where

import Data.Complex

-- calculates the square of a complex number
sq :: (RealFloat a) => Complex a -> Complex a
sq z = mult z z

-- multiplies two complex numbers
mult :: (RealFloat a) => Complex a -> Complex a -> Complex a
mult (r1 :+ i1) (r2 :+ i2) = r3 :+ i3
  where
  r3 = r1*r2-i1*i2
  i3 = r1*i2+r2*i1

-- adds two complex numbers
add :: (RealFloat a) => Complex a -> Complex a -> Complex a
add (r1 :+ i1) (r2 :+ i2) = r3 :+ i3
  where
  r3 = r1+r2
  i3 = i1+i2

-- the actual calculation z = z^2+c
mandel' :: (RealFloat a) => Complex a -> Complex a-> Complex a
mandel' c z = add (sq z) c

-- recursiv calculation of a color value.
-- ends when magnitude(z)>zmax or num of interations reached
calc_mandel' :: (RealFloat a) => a -> Int -> Complex a -> Int -> Complex a -> Int
calc_mandel' zmax nmax c n z
  | n >= nmax  = n
  | absz > zmax = n
  | otherwise   = calc_mandel' zmax nmax c (n+1) z'
  where
  absz = magnitude z'
  z' = mandel' c z

-- the calc funtion for mandel  
calc_mandel :: (RealFloat a) => a -> Int -> Complex a -> Int
calc_mandel zmax nmax c = calc_mandel' zmax nmax c 0 znull

-- the null value in complex
znull :: (RealFloat a) => Complex a
znull = 0 :+ 0

-- interations
iterations :: Int
iterations = 100

-- calculate the value at pixel x y
pixelAt' :: (RealFloat a) => a -> Int -> a -> a -> Int
pixelAt' zmax nmax x y = calc_mandel zmax nmax (x :+ y)	

-- calculate the value at pixel x y with default values
pixelAt :: (RealFloat a) => a -> a -> Int
pixelAt = pixelAt' 20.0 iterations
