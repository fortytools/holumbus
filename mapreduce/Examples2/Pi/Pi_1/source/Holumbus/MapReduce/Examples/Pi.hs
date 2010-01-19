module Holumbus.MapReduce.Examples.Pi (
  piMap
, piReduce
) where

import Holumbus.MapReduce.Examples.SimpleDMapReduceIO
import System.Random.Mersenne
import Data.Maybe

calcSample :: MTGen -> () -> IO (Maybe Int)
calcSample mtg _ = do
  -- make random values
  (x:y:_)::[Double] <- randoms mtg
  
  if (x*x+y*y) <= 1
    then return (Just 1)
    else return Nothing

{-
  The mapping function
-}
piMap :: MapFunction Options K1 V1 K2 V2
piMap _env _opts key num_of_samples = do
  -- create random generator
  mtg <- newMTGen Nothing
  samples <- mapM (calcSample mtg) [() | _ <- [1..num_of_samples]]

  return [(key, catMaybes samples)]

{-
 The reduce function
-}
piReduce :: ReduceFunction Options K2 V3 V4
piReduce _env _opts _ = return . Just . sum . concat
