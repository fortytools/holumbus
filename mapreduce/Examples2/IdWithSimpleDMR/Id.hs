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
idMap :: SimpleMapFunction Int Int Int Int
idMap = (,)

{-
 The reduce function
-}
idReduce :: SimpleReduceFunction Int Int Int
idReduce key = head

