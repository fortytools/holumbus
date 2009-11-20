module Main
(
   main
)
where


import Examples2.CountWithSimpleIO2.DCount
import System.Log

main :: IO ()
main = worker countMap countReduce [("Holumbus.MapReduce.Types", DEBUG)]
