module Main
(
   main
)
where


import Holumbus.MapReduce.Examples.SimpleDMapReduceIO
import Holumbus.MapReduce.Examples.Pi
import System.Log

main :: IO ()
main = worker piMap piReduce [("Holumbus.MapReduce.Types",INFO),("measure",ERROR)]
