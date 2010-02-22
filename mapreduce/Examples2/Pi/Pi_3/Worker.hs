module Main
(
   main
)
where


import Holumbus.MapReduce.Examples.SimpleDMapReduceIO
import Holumbus.MapReduce.Examples.Pi
import System.Log
import System.Random.Mersenne
import System.Environment

main :: IO ()
main = do
  -- get the random number from command line
  ( rnd : _ ) <- getArgs

  -- wrap as seed
  let seed = Just (read rnd)

  -- create a new random gen
  mtg <- newMTGen seed

  -- start the worker
  worker (piMap mtg) piReduce [("Holumbus.MapReduce.Types",INFO),("measure",ERROR)]
