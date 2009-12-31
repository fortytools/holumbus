module Main
where

import Crawl2.Examples2.MRIndexer
import Holumbus.Distribution.SimpleDMapReduceIO

main :: IO ()
main = worker idxMap idxReduce [("measure.getMessage",DEBUG),("measure.readStorage",DEBUG),("measure.putMessage",DEBUG)]
