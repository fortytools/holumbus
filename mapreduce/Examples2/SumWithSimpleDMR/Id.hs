module Examples2.IdWithSimpleDMR.Id
(
   idMap
 , idReduce
)
where

import Examples2.SimpleMR.SimpleDMapReduce

{-
  The mapping function
-}
idMap :: SimpleMapFunction Int [Int] Int Int
idMap key ls = (key, sum ls)

{-
 The reduce function
-}
idReduce :: SimpleReduceFunction Int Int Int
idReduce key = sum

