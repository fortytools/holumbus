module Examples2.CountWithSimpleIO.DCount
(
   countMap
 , countReduce
)
where

import Holumbus.Distribution.SimpleDMapReduceIO
import Data.List
import Control.Parallel.Strategies
import System.Random

{-

type MapFunction a k1 v1 k2 v2 = ActionEnvironment -> a -> k1 -> v1 -> IO [(k2, v2)]
-}
countMap :: MapFunction [String] Int String Int Int
countMap _env wordsToCount k1 v1 = do
--  putStrLn $ "map: ("++show k1++" / "++(show . length $ v1)++" )"
--  let result =  zip (repeat k1) (map counts v1)
--  return $ rnf result `seq` result
  let c = counts v1
  key <- getStdRandom (randomR (1,10))
  return [(key,c)]
  where
  counts :: String -> Int
  counts word = (b2int . or . map (==word)) wordsToCount
  b2int True  = 1
  b2int False = 0

{-

type ReduceFunction a k2 v2 v3 = ActionEnvironment -> a -> k2 -> [v2] -> IO (Maybe v3)
-}
countReduce :: ReduceFunction [String]  Int Int Int
countReduce _env _opts k2 v2s = do
  putStrLn $ "reduce: ("++show k2++" / "++(show . length $ v2s)++" )"
  return . Just . sum $ v2s
