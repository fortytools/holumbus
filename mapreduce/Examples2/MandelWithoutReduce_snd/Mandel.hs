module Examples2.MandelWithoutReduce_snd.Mandel
(
    pixelAt
)
where

data Complex = !Double :+ !Double

magnitude :: Complex -> Double
magnitude (x:+y) = sqrt $ (x*x)+(y*y)
    where
    x2 = x*x
    y2 = y*y


-- calculates the square of a complex number
sq :: Complex -> Complex
sq z = mult z z

-- multiplies two complex numbers
mult :: Complex -> Complex -> Complex
mult (r1 :+ i1) (r2 :+ i2) = r3 :+ i3
  where
  r3 = r1*r2-i1*i2
  i3 = r1*i2+r2*i1

-- adds two complex numbers
add :: Complex -> Complex -> Complex
add (r1 :+ i1) (r2 :+ i2) = r3 :+ i3
  where
  r3 = (r1+r2)
  i3 = (i1+i2)

-- the actual calculation z = z^2+c
mandel' :: Complex -> Complex-> Complex
mandel' c z = add (sq z) c

-- recursiv calculation of a color value.
-- ends when magnitude(z)>zmax or num of interations reached
calc_mandel' :: Double -> Int -> Complex -> Int -> Complex -> Int
calc_mandel' zmax nmax c n z 
  | n >= nmax  = n
  | absz > zmax = n
  | otherwise   = calc_mandel'  zmax nmax c (n+1) z'
  where
    absz = magnitude z'
    z'   = mandel' c z

-- the calc funtion for mandel  
calc_mandel :: Double -> Int -> Complex -> Int
calc_mandel zmax nmax c = calc_mandel' zmax nmax c 0 znull

-- the null value in complex
znull :: Complex
znull = 0 :+ 0

-- interations
iterations :: Int
iterations = 100

-- calculate the value at pixel x y
pixelAt :: Double -> Int -> Double -> Double -> Int
pixelAt zmax nmax x y = calc_mandel zmax nmax (x :+ y)	
