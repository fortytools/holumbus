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
sumMap :: SimpleMapFunction () Int [Int] Int Int
sumMap _ key ls = (key, sum ls)

{-
 The reduce function
-}
sumReduce :: SimpleReduceFunction () Int Int Int
sumReduce _ _ = sum