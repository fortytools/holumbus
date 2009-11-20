module Main
(
   main
)
where

import Examples2.MandelWithoutReduce.MapReduceFacade
import Examples2.MandelWithoutReduce.DMandel

main :: IO ()
main = worker mandelMap mandelReduce [("Holumbus.MapReduce.Types",INFO)]
