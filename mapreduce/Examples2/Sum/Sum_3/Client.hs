module Main
(
   main
)
where


import Holumbus.MapReduce.Examples.SimpleDMapReduceIO 
import Holumbus.MapReduce.Examples.Sum
import System.Environment
import Control.Parallel.Strategies
import Holumbus.Common.FileHandling
import GHC.Int
import Holumbus.Common.Logging

m c = 10^c
n c = 10^(7-c) - 1 

main :: IO ()
main = do
  putTimeStamp "Client Begin"
  ( c' : triplet : [] ) <- getArgs
  let (splitters,mappers,reducers) = read triplet
  initializeLogging [("Holumbus",ERROR),("measure",ERROR)]
  putTimeStamp "Client Begin MR"
  result <- client sumMap sumReduce () (splitters,mappers,reducers) (t (read c'))
  putTimeStamp "Client End MR"
  putStrLn . show $ result
  putStrLn . show . sum . map snd $ result
  putTimeStamp "Client End"
  where
  t :: Int64 -> [[(K1,V1)]]
  t c =  [[(fromIntegral i,[i*(m c)+1..(i+1)*(m c)])]| i<-[0..(n c)]]
