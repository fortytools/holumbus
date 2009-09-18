module Main
(
   main
)
where


import Holumbus.Distribution.SimpleDMapReduceIO
import Examples2.CountWithSimpleIO.DCount

main :: IO ()
main = worker countMap countReduce
