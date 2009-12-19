module Main
(
   main
)
where

import Holumbus.MapReduce.Examples.MandelbrotSet.SimpleDMapReduceIO 
import Holumbus.MapReduce.Examples.MandelbrotSet.DMandel

main :: IO ()
main = worker splitF mapF reduceF
  [
  ("Holumbus.MapReduce.Examples",DEBUG)
  ,("Holumbus.MapReduce.Types",INFO)
  ,("Holumbus.MapReduce.Examples2",INFO)
  ]
