module Main
(
   main
)
where


import Holumbus.MapReduce.Examples.Count.SimpleDMapReduceIO
import Holumbus.MapReduce.Examples.Count.DCount
import System.Log

main :: IO ()
main = worker countMap countReduce [ ("Holumbus.MapReduce.Types", INFO)
                                    ,("measure.putMessage", ERROR)
                                    ,("measure.getMessage", ERROR)
                                    ,("measure.readStorage", ERROR)
                                    ,("count", DEBUG)]
