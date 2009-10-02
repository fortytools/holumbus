module Main
(
   main
)
where


import Holumbus.Distribution.SimpleDMapReduceIO
import Examples2.SumWithSimpleIO.Sum
import System.Log

main :: IO ()
main = worker sumMap sumReduce [("Holumbus.MapReduce.Types",INFO)]
