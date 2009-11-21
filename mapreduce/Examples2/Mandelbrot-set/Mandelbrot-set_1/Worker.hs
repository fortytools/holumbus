module Main
(
   main
)
where

import Holumbus.MapReduce.Examples.MandelbrotSet.SimpleDMapReduceIO 
import Holumbus.MapReduce.Examples.MandelbrotSet.DMandel

main :: IO ()
main = worker mandelMap mandelReduce [("Holumbus.MapReduce.Types",INFO)]
