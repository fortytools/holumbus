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

-- holumbus libs
import Holumbus.Common.Logging

-- system libs
import System.Environment
import System.Log.Logger
import Data.List

localLogger = "Holumbus.MapReduce.Examples2.Client"

main :: IO ()
main = do
  initializeFileLogging "/dev/stdout" ([(localLogger, ERROR),("Holumbus.Network.DoWithServer",INFO),("measure",ERROR)])
  putTimeStamp "Begin Client"
  -- read command line arguments
  (filename : firstsplit : quintet : duo : [] ) <- getArgs
  let (split,w,h,zmax,iterations) = read quintet
      ; (splitters, mappers) = read duo
      ; list = map (:[]) $ part (read firstsplit) h $ pixels w h
    
  -- call map reduce
  putTimeStamp "Begin Client MR"
  result <- client splitF mapF reduceF (split,w,h,zmax,iterations) (splitters, mappers) list
  putTimeStamp "End Client MR"
  debugM localLogger $ show result
  putTimeStamp "Begin Save"
  let image = concatMap snd . sortBy sortImage $ result
  --debugM localLogger $ show image 
  -- make the image
  saveImage (Geo w h) (concatMap snd $ image) filename
  putTimeStamp "End Save"
  putTimeStamp "End Client"

{-
 generate the pixlist
-}
pixels :: Int -> Int -> V1
pixels w h = [(y,[0..w-1])|y<-[0..h-1]]
