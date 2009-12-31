module Holumbus.MapReduce.Examples.Count.DCount
(
   countMap
 , countReduce
 , K1
 , V1
)
where

import Holumbus.Distribution.SimpleDMapReduceIO
import Control.Parallel.Strategies
import Holumbus.MapReduce.Types
import Data.Maybe (mapMaybe)

type Options = [String]
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
countMap _env wordsToCount k1 v1 = (return . zip (repeat k1) . mapMaybe counts) v1
--   let cs= map (\x -> let x' = counts x in (k1,x')) v1 in rnf cs `seq` return cs
  where
  counts :: String -> Maybe Int
  counts word = (b2int . or . map (==word)) wordsToCount
  b2int True  = Just 1
  b2int False = Nothing
  

{-

type ReduceFunction a k2 v2 v3 = ActionEnvironment -> a -> k2 -> [v2] -> IO (Maybe v3)
-}
countReduce :: ReduceFunction Options K2 V2 V4
countReduce _env _opts _k2 = return . Just . sum
