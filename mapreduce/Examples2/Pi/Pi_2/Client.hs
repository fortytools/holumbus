module Main
(
   main
)
where


import Holumbus.MapReduce.Examples.Pi
import Holumbus.MapReduce.Examples.SimpleDMapReduceIO
import System.Environment
import Holumbus.Common.Logging

main :: IO ()
main = do
  putTimeStamp "Client Begin"
  ( num_of_samples' : triplet : [] ) <- getArgs
  let (mappers,reducers) = read triplet;
      num_of_samples = read num_of_samples'

  initializeLogging [("Holumbus",ERROR),("measure",ERROR)]
  putTimeStamp "Client Begin MR"
  let t' = t mappers (num_of_samples `div` mappers)
  putStrLn . show $ t'

  result <- client piMap piReduce () (mappers,reducers) t'

  putTimeStamp "Client End MR"
  putStrLn . show $ result
  let inside = sum . map snd $ result;
      pi = 4* (fromIntegral inside) / (read num_of_samples')

  putStrLn . show $ inside

  putStrLn ( "Pi is ~"++show pi)
 
  

  putTimeStamp "Client End"
  where
  t :: V1 -> K1 -> [[(K1,V1)]]
  t m s = [[(i,s)]|i<-[1..m]]
