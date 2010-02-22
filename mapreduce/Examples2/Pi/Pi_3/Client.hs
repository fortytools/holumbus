module Main ( main ) where

import Holumbus.MapReduce.Examples.Pi
import Holumbus.MapReduce.Examples.SimpleDMapReduceIO
import System.Environment
import Holumbus.Common.Logging
import System.Random.Mersenne

main :: IO ()
main = do
  putTimeStamp "Client Begin"
  ( num_of_samples' : tuple : [] ) <- getArgs
  let (mappers,reducers) = read tuple;
      num_of_samples = read num_of_samples'

  initializeLogging [("Holumbus",ERROR),("measure",ERROR)]
  putTimeStamp "Client Begin MR"
  let c = if mappers == 1 then 1 else mappers - 1
      n = (num_of_samples `div` c);
      rest = (num_of_samples `mod` c);
      input = ([(0,rest)]:(zipWith (\i n' -> [(i,n')]) [1..] (replicate c n)))

  mtg <- newMTGen Nothing
  result <- client (piMap mtg) piReduce () (mappers,reducers) input

  putTimeStamp "Client End MR"
  let inside = sum . map snd $ result;
      pi = 4* (fromIntegral inside) / (read num_of_samples')

  putStrLn ( "Pi is ~"++show pi)
  putTimeStamp "Client End"
