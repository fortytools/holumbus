module Main
(
   main
)
where

-- the mr facade
import Examples2.MandelWithoutReduce_snd.MapReduceFacade

-- mandel libs
import Examples2.MandelWithoutReduce_snd.DMandel
import Examples2.MandelWithoutReduce_snd.ImageTypes

-- system libs
import System.Environment
import Data.List

main :: IO ()
main = do 
  -- read command line arguments
  (filename : quartet : triplet : [] ) <- getArgs
  let (w,h,zmax,iterations) = read quartet
      ; (splitters,mappers,reducers) = read triplet
      ; list = partition' (pixels w h) [[]|_<-[1..splitters]]
    
  -- call map reduce ::  [(Int, (Int, Lightness))]
  result <- client mandelMap mandelReduce (w,h,zmax,iterations) (splitters,mappers,reducers) list
  
  -- make the image
  let pix = (map snd . sortBy sortPixels . map snd) result
  saveImage (Geo w h) pix filename

  
{-
 generate the pixlist
-}
pixels :: Int -> Int -> [(Int,(Int,Int))]
pixels w h = zip [0..] ([(x,y) | y<-[0..h-1], x<-[0..w-1]])

{-
  order function the pixels 
-}
sortPixels :: (Ord k) => (k,v) -> (k,v) -> Ordering
sortPixels (k1,_) (k2,_) 
  | k1 > k2 = GT
  | k1 < k2 = LT
  | otherwise = EQ
