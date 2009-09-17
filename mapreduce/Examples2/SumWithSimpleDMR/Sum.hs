module Examples2.SumWithSimpleDMR.Sum
(
   sumMap
 , sumReduce
)
where

import Holumbus.Distribution.SimpleDMapReduce

{-
  The mapping function
-}
sumMap :: SimpleMapFunction () Integer [Integer] Integer Integer
sumMap _ key ls = (key, sum ls)

{-
 The reduce function
-}
sumReduce :: SimpleReduceFunction () Integer Integer Integer
sumReduce _ _ = sum
