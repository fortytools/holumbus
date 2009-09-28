module Examples2.MandelWithSimpleIO.ImageMandel
where

import Examples2.MandelWithSimpleIO.Mandel
import Examples2.MandelWithSimpleIO.ImageTypes

x // y = fromIntegral x / fromIntegral y


-- calculate the mandel value at coord (x,y)
imageMandel :: Geo -> Double -> Int -> Channel
imageMandel (Geo w h) zmax iter x y = (fromIntegral $ pixelAt zmax iter x' y') / 100
  where
  x' = (fromIntegral $ x - w2*2) // w2
  y' = (fromIntegral $ h2 - y) // h2
  w2 = (w `div` 3)
  h2 = (h `div` 2)
