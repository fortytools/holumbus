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
   ("measure",ERROR)
  ,("Holumbus.MapReduce.Types",INFO)
  ,("Holumbus.MapReduce.Examples2",DEBUG)
  ]
