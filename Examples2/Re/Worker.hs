module Main
where

import Examples2.Re.MRIndexer
import Holumbus.Distribution.SimpleDMapReduceIO

main :: IO ()
main = worker idxMap idxReduce [("measure.getMessage",DEBUG),("measure.readStorage",DEBUG),("measure.putMessage",DEBUG)]
