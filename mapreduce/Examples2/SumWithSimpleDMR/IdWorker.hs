module Main
(
   main
)
where


import Examples2.SimpleMR.SimpleDMapReduce
import Examples2.IdWithSimpleDMR.Id

main :: IO ()
main = simpleWorker idMap idReduce
