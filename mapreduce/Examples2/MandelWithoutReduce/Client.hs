module Main
(
   main
)
where

-- the mr facade
import Examples2.MandelWithoutReduce.MapReduceFacade

-- mandel libs
import Examples2.MandelWithoutReduce.DMandel
import Examples2.MandelWithoutReduce.ImageTypes

-- system libs
import System.Environment
import Data.List
import Data.Time.Clock.POSIX
import Holumbus.Common.FileHandling

main :: IO ()
main = do 
  -- read command line arguments
  (filename : quartet : triplet : [] ) <- getArgs
  t <- getPOSIXTime
  let (w,h,zmax,iterations) = read quartet
      ; (splitters,mappers,reducers) = read triplet
--      ; list = let p=partition' (pixels w h) [[]|_<-[1..splitters]] in rnf p `seq` p
      ; list = partition' (pixels w h) [[]|_<-[1..splitters]]
  writeToListFile "/tmp/blub.bin" list
    
  -- call map reduce
  result <- client mandelMap mandelReduce (w,h,zmax,iterations) (splitters,mappers,reducers) list
  
  -- make the image
  let pix = (concat . map snd . sortBy sortPixels) result
  saveImage (Geo w h) pix filename

  
{-
 generate the pixlist
-}
pixels :: Int -> Int -> [(Int,[Int])]
pixels w h = [(y,[0..w-1])|y<-[0..h-1]]

{-
  order function the pixels 
-}
sortPixels :: (Ord k) => (k,v) -> (k,v) -> Ordering
sortPixels (k1,_) (k2,_) 
  | k1 > k2 = GT
  | k1 < k2 = LT
  | otherwise = EQ
