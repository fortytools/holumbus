module Main
(
   main
)
where

-- the mr facade
import Holumbus.MapReduce.Examples.MandelbrotSet.SimpleDMapReduceIO

-- mandel libs
import Holumbus.MapReduce.Examples.MandelbrotSet.DMandel
import Holumbus.MapReduce.Examples.MandelbrotSet.ImageTypes

-- system libs
import System.Environment
import Data.List
import Data.Time.Clock.POSIX
import Holumbus.Common.FileHandling

main :: IO ()
main = do 
  -- read command line arguments
  putTimeStamp "Begin Client"
  (filename : quartet : triplet : [] ) <- getArgs
  t <- getPOSIXTime
  let (w,h,zmax,iterations) = read quartet
      ; (mappers,reducers) = read triplet
      ; list = devide (div h mappers) (pixels w h) 
  --writeToListFile "/dev/null" list
    
  -- call map reduce
  putTimeStamp "Begin Client MR"
  result <- client mandelMap mandelReduce (w,h,zmax,iterations) (mappers,reducers) list
  putTimeStamp "End Client MR"
  
  -- make the image
  putTimeStamp "Begin Save"
  let pix = (concatMap snd . concatMap snd . sortBy sortPixels) result -- [(Int,[(Int,[Lightness])])]
  saveImage (Geo w h) pix filename
  putTimeStamp "End Save"
  putTimeStamp "End Client"

{-
  devide image into coherent blocks
-}
devide :: Int -> V1 -> [[(K1,V1)]]
devide = devide' 0 []
  where
  devide' key xss n [] = xss
  devide' key xss n xs = devide' key' xss' n rest
    where
    key' = key + 1
    xss' = ([(key,xs')]:xss)
    (xs',rest) = splitAt n xs -- ( [(Int,[Int])] , [(Int,[Int])] )
  
{-
 generate the pixlist
-}
pixels :: Int -> Int -> V1
pixels w h = [(y,[0..w-1])|y<-[0..h-1]]

{-
  order function the pixels 
-}
sortPixels :: (Ord k) => (k,v) -> (k,v) -> Ordering
sortPixels (k1,_) (k2,_) 
  | k1 > k2 = GT
  | k1 < k2 = LT
  | otherwise = EQ
