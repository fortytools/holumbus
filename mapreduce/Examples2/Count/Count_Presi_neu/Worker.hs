module Main
(
   main
)
where


import Holumbus.MapReduce.Examples.Count.SimpleDMapReduceIO
import Holumbus.MapReduce.Examples.Count.DCount
import System.Log
import Control.Parallel.Strategies
import qualified Data.ByteString.Lazy.Char8 as C

instance NFData C.ByteString where
  rnf _ = ()

main :: IO ()
main = worker countMap countReduce [ ("Holumbus.MapReduce.Types", INFO)
                                    ,("measure.putMessage", ERROR)
                                    ,("measure.getMessage", ERROR)
                                    ,("measure.readStorage", ERROR)
                                    ,("count", ERROR)]
