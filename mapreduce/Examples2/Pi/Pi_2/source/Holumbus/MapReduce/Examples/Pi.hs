module Holumbus.MapReduce.Examples.Pi (
  piMap
, piReduce
) where

import Holumbus.MapReduce.Examples.SimpleDMapReduceIO
import Holumbus.MapReduce.Types
import System.Random.Mersenne
import Data.Maybe
import Control.Parallel.Strategies
import Control.Parallel

mapP :: [Double] -> ((Double,Double) -> () -> Maybe Int) -> [()] -> [Maybe Int]
mapP _ _ []           = []
mapP (x:y:_) f (w:[])       = [f (x,y) w]
mapP (x:y:x':y':_) f (u:v:[])     = fv `par` [f (x,y) u,fv]
    where fv = f (x',y') v
    
mapP (x:y:x':y':x'':y'':_) f (u:v:w:[])   = fv `par` fw `par` [f (x,y) u,fv,fw]
    where fv = f (x',y') v
          fw = f (x'',y'') w
                                         
mapP (x:y:x':y':x'':y'':x''':y''':rest) f (m:n:o:p:ps) = fn `par` fo `par` fp `par` (f (x,y) m:fn:fo:fp : (mapP rest f ps))
    where fn = f (x',y') n
          fo = f (x'',y'') o
          fp = f (x''',y''') p

calcSample' :: (Double,Double) -> () -> Maybe Int
calcSample' (x,y) _ = if (x*x+y*y) <= 1
  then (Just 1)
  else Nothing

{-
  The mapping function
-}
piMap :: MapFunction Options K1 V1 K2 V2
piMap env _opts key num_of_samples = do
  -- create random generator
  mtg <- newMTGen Nothing
  rands <- randoms mtg
  let samples = mapP rands calcSample' [() | _ <- [1..num_of_samples]]

  return [(key', catMaybes samples)]
  where
  key' = case (td_PartValue . ae_TaskData $ env) of
    (Just n') -> mod key n'
    Nothing   -> key

{-
 The reduce function
-}
piReduce :: ReduceFunction Options K2 V3 V4
piReduce _env _opts _ = return . Just . sum . concat
