module Main
(
   main
)
where


import Holumbus.Distribution.SimpleDMapReduce
import Examples2.IdWithSimpleDMR.Id

main :: IO ()
main = simpleWorker idMap idReduce
