module Holumbus.MapReduce.Examples.Count.DCount
(
   countMap
 , countReduce
)
where

import Holumbus.Distribution.SimpleDMapReduceIO
import Control.Parallel.Strategies
import Holumbus.MapReduce.Types

instance Hash Integer where
  hash n k = mod (fromIntegral k) n

{-

type MapFunction a k1 v1 k2 v2 = ActionEnvironment -> a -> k1 -> v1 -> IO [(k2, v2)]
-}
countMap :: MapFunction [String] Integer [String] Integer Integer
countMap _env wordsToCount k1 v1 = do
--  putStrLn $ "map: ("++show k1++" / "++(show . length $ v1)++" )"
--  let result =  zip (repeat k1) (map counts v1)
--  return $ rnf result `seq` result
--  let c = counts v1
--  return [(c,c)]
  let cs= map (\x -> let x' = counts x in (k1,x')) v1 in rnf cs `seq` return cs 
  where
  counts :: String -> Integer
  counts word = (b2int . or . map (==word)) wordsToCount
  b2int True  = 1
  b2int False = 0

{-

type ReduceFunction a k2 v2 v3 = ActionEnvironment -> a -> k2 -> [v2] -> IO (Maybe v3)
-}
countReduce :: ReduceFunction [String]  Integer Integer Integer
countReduce _env _opts _k2 v2s = let b = Just . sum $ v2s in rnf b `seq` return b
