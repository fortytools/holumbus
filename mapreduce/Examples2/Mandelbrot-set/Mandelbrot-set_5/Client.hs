module Main
(
   main
)
where

-- the mr facade
import Holumbus.MapReduce.Examples.MandelbrotSet.SimpleDMapReduceIO

-- mandel libs
import Holumbus.MapReduce.Examples.MandelbrotSet.DMandel
import Holumbus.MapReduce.Examples.MandelbrotSet.ImageTypes hiding (Image)

-- system libs
import System.Environment
import System.Log.Logger
import Data.List

localLogger = "Holumbus.MapReduce.Examples2.Client"

main :: IO ()
main = do
  -- read command line arguments
  (filename : quintet : duo : [] ) <- getArgs
  let (w,h,zmax,iterations) = read quintet
      ; (splitters, mappers) = read duo
      ; list = map (:[]) $ part splitters h $ pixels w h
    
  -- call map reduce
  result <- client splitF mapF reduceF (w,h,zmax,iterations) (splitters, mappers) list
  let image = concatMap snd . sortBy sortImage $ result
  debugM localLogger $ show image 
  -- make the image
  saveImage (Geo w h) (concatMap snd $ image) filename

{-
 generate the pixlist
-}
pixels :: Int -> Int -> V1
pixels w h = [(y,[0..w-1])|y<-[0..h-1]]
