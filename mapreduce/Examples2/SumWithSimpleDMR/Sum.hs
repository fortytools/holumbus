module Examples2.IdWithSimpleDMR.Id
(
   idMap
 , idReduce
)
where

import Holumbus.Distribution.SimpleDMapReduce

{-
  The mapping function
-}
idMap :: SimpleMapFunction () Int [Int] Int Int
idMap _ key ls = (key, sum ls)

{-
 The reduce function
-}
idReduce :: SimpleReduceFunction () Int Int Int
idReduce _ _ = sum

