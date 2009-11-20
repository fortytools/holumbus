module Main
(
   main
)
where


import Examples2.MandelWithoutReduce3.MapReduceFacade
import Examples2.MandelWithoutReduce3.DMandel

main :: IO ()
main = worker mandelMap mandelReduce [("Holumbus.MapReduce.Types",INFO)]
