module Examples2.SumWithSimpleDMR.Sum
(
   sumMap
 , sumReduce
)
where

import Holumbus.Distribution.SimpleDMapReduceIO
import Holumbus.MapReduce.Types

{-
  The mapping function
-}
sumMap :: MapFunction () Int [Integer] Int Integer
sumMap env _opts key ls = return [(mod key n, sum ls)]
  where
  n = case (td_PartValue . ae_TaskData $ env) of
        (Just n') -> n'
        Nothing   -> key+1 -- so mod will give the key as answer

{-
 The reduce function
-}
sumReduce :: ReduceFunction () Int Integer Integer
sumReduce _env _opts _ = return . Just . sum
