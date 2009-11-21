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

  (filename : quartet : triplet : [] ) <- getArgs
  t <- getPOSIXTime
  let (w,h,zmax,iterations) = read quartet
      ; (splitters,mappers,reducers) = read triplet
--      ; list = let p=partition' (pixels w h) [[]|_<-[1..splitters]] in rnf p `seq` p
      ; list = devide (div h mappers) (pixels w h) -- partition' (pixels w h) [[]|_<-[1..splitters]]
  writeToListFile "/dev/null" list
    
  -- call map reduce
  result <- client mandelMap mandelReduce (w,h,zmax,iterations) (splitters,mappers,reducers) list
  
  -- make the image
  let pix = (concat . map snd . concat . map snd . sortBy sortPixels) result -- [(Int,[(Int,[Lightness])])]
  saveImage (Geo w h) pix filename

{-
  devide image into coherent blocks
-}
devide :: Int -> [(Int,[Int])] -> [[(Int,[(Int,[Int])])]]
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
