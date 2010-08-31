module Main
(
   main
)
where


import Holumbus.MapReduce.Examples.SimpleDMapReduceIO 
import Holumbus.MapReduce.Examples.Sum
import System.Environment
import Control.DeepSeq
import Holumbus.Common.FileHandling
import GHC.Int
import Holumbus.Common.Logging

main = do
  putTimeStamp "Client Begin"
  ( c' : triplet : [] ) <- getArgs
  let (splitters,mappers,reducers) = read triplet
  initializeLogging [("Holumbus",ERROR),("measure",ERROR)]
  putTimeStamp "Client Begin MR"
  result <- client sumMap sumReduce () (splitters,mappers,reducers) (t splitters)
  putTimeStamp "Client End MR"
  putStrLn . show $ result
  putStrLn . show . sum . map snd $ result
  putTimeStamp "Client End"
  where
  t :: Int -> [[(K1,V1)]]
  t 10 = [[(i,[1..2*10^6])] | i<-[0..9]]
  t 20 = [[(i,[1..10^6])] | i<-[0..19]]
  t _  = error "not configured"
