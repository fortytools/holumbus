module Main
(
   main
)
where


import Holumbus.Distribution.SimpleDMapReduce
import Examples2.SumWithSimpleDMR.Sum

main :: IO ()
main = simpleWorker sumMap sumReduce
