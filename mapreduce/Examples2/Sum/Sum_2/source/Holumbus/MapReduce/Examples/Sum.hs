module Holumbus.MapReduce.Examples.Sum 
where

import Holumbus.Distribution.SimpleDMapReduceIO
import Holumbus.MapReduce.Types
import GHC.Int

type Options=()

type K1 = Int
type V1 = [Int64]
type K2 = Int
type V2 = Int64
type V3 = V2
type V4 = Int64

{-
  The mapping function
-}
sumMap :: MapFunction Options K1 V1 K2 V2
sumMap env _opts key ls = return [(key, sum ls)]

{-
 The reduce function
-}
sumReduce :: ReduceFunction Options K2 V3 V4
sumReduce _env _opts _ = return . Just . sum
