module Main
(
   main
)
where


import Holumbus.MapReduce.Examples.SimpleDMapReduceIO 
import Holumbus.MapReduce.Examples.Sum
import System.Log

main :: IO ()
main = worker sumMap sumReduce [("sum",ERROR),("Holumbus.MapReduce.Types",ERROR),("measure",ERROR),("Holumbus.FileSystem",ERROR),("Holumbus.MapReduce",INFO),("Holumbus.Network",ERROR)]
