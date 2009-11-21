module Main
(
   main
)
where


import Holumbus.Distribution.SimpleDMapReduceIO
import Holumbus.MapReduce.Examples.Sum
import System.Log

main :: IO ()
main = worker sumMap sumReduce [("Holumbus.MapReduce.Types",INFO)]
