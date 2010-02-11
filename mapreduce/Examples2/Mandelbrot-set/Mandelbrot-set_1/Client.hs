module Main
(
   main
)
where

-- the mr facade
import Holumbus.Distribution.SimpleDMapReduceIO

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
      ; (splitters,mappers,reducers) = read triplet
--      ; list = let p=partition' (pixels w h) [[]|_<-[1..splitters]] in rnf p `seq` p
      ; list = partition' (pixels w h) [[]|_<-[1..splitters]]
--  writeToListFile "/tmp/blub.bin" list
    
  -- call map reduce
  putTimeStamp "Begin Client MR"
  result <- client mandelMap mandelReduce (w,h,zmax,iterations) (splitters,mappers,reducers) list
  putTimeStamp "End Client MR"

  -- make the image
  putTimeStamp "Begin Save"
  let pix = (concat . map snd . sortBy sortPixels) result
  saveImage (Geo w h) pix filename
  putTimeStamp "End Save"
  putTimeStamp "End Client"

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
