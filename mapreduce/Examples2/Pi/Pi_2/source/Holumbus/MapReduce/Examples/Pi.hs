module Holumbus.MapReduce.Examples.Pi (
  piMap
, piReduce
) where

import Holumbus.MapReduce.Examples.SimpleDMapReduceIO
import Holumbus.MapReduce.Types
import System.Random.Mersenne
import Data.Maybe
import Control.DeepSeq
import Control.Parallel

calcSample :: Double -> Double -> Bool
calcSample x y = (x*x+y*y) <= 1

mapP :: Int -> (Double -> Double -> Bool) -> [Double] -> [Bool]
mapP 0 _ _ =  []
mapP n f (x1:y1:xs) = (f1: (mapP (n-1) f xs) )
  where
  f1 = f x1 y1

{-
  The mapping function
-}
piMap :: MapFunction Options K1 V1 K2 V2
piMap env _opts key num_of_samples = do
  -- create random generator
  mtg <- newMTGen Nothing
  rands <- randoms mtg
  let samples = mapP num_of_samples calcSample rands

  return [(key', filter (==True) samples)]
  where
  key' = case (td_PartValue . ae_TaskData $ env) of
    (Just n') -> mod key n'
    Nothing   -> key

{-
 The reduce function
-}
piReduce :: ReduceFunction Options K2 V3 V4
piReduce _env _opts _ = return . Just . sum . map length
