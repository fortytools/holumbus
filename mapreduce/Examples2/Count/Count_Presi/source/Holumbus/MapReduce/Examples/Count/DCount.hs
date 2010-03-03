module Holumbus.MapReduce.Examples.Count.DCount
(
   countMap
 , countReduce
 , K1
 , V1
)
where

import Holumbus.MapReduce.Examples.Count.SimpleDMapReduceIO
import Control.Parallel.Strategies
import Holumbus.MapReduce.Types
import Data.Maybe (mapMaybe)

type Options = String
type K1 = Int
type V1 = [String]
type K2 = Int
type V2 = Int
type V3 = V4
type V4 = Int

{-

type MapFunction a k1 v1 k2 v2 = ActionEnvironment -> a -> k1 -> v1 -> IO [(k2, v2)]
-}
countMap :: MapFunction Options K1 V1 K2 V2
countMap env term key line = do
  debugM "count" $ "(k1,v1)" ++ show (key,line)
  debugM "count" $ "(k2,v2)" ++ show (newkey,newvalue)
  return [(newkey, newvalue)]
  where
  newkey = case (td_PartValue . ae_TaskData $ env) of
        (Just n') -> mod key n'
        Nothing   -> key 
 
  newvalue = (length . filter (==term)) line 

{-

type ReduceFunction a k2 v2 v3 = ActionEnvironment -> a -> k2 -> [v2] -> IO (Maybe v3)
-}
countReduce :: ReduceFunction Options K2 V2 V4
countReduce _env _opts k2 values = do
  debugM "count" $ "(k2,v2)" ++ show (k2,values)
  return . Just . sum $ values
