module Main
(
   main
)
where


import Holumbus.MapReduce.Examples.Count.DCount 
import System.Log

main :: IO ()
main = worker countMap countReduce [("Holumbus.MapReduce.Types", DEBUG)]
