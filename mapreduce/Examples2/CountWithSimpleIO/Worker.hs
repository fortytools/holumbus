module Main
(
   main
)
where


import Holumbus.Distribution.SimpleDMapReduceIO
import Examples2.CountWithSimpleIO.DCount
import System.Log

main :: IO ()
main = worker countMap countReduce [("Holumbus.MapReduce.Types", INFO)]
