module Main
(
   main
)
where


import Holumbus.Distribution.SimpleDMapReduceIO
import Examples2.MandelWithSimpleIO.DMandel

main :: IO ()
main = worker mandelMap mandelReduce [("Holumbus.MapReduce.Types",INFO)]
