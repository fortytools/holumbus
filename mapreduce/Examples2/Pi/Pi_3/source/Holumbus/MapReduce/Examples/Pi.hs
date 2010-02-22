module Holumbus.MapReduce.Examples.Pi (
  piMap
, piReduce
) where

import Holumbus.MapReduce.Examples.SimpleDMapReduceIO
import Holumbus.MapReduce.Types
import System.Random.Mersenne
import qualified Data.ByteString.Lazy as B
import Data.Binary (encode)
import Data.Word (Word8)

w0 :: Word8
w0 = fromIntegral 0

w1 :: Word8
w1 = fromIntegral 1

calcSample :: Double -> Double -> Word8
calcSample x y = if (x*x+y*y) <= 1 then w1 else w0

mapP :: V1 -> (Double -> Double -> Word8) -> [Double] -> B.ByteString
mapP n f xs = res `seq` B.pack res
  where
  res = mapP' n f xs

  mapP' :: V1 -> (Double -> Double -> Word8) -> [Double] -> [Word8]
  mapP' 0 _ _ = []
  mapP' n f (x1:y1:xs) = (f1:mapP' (n-1) f xs)
    where
    f1 = f x1 y1

{-
  The mapping function
-}
piMap :: MTGen -> MapFunction Options K1 V1 K2 V2
piMap mtg env _opts key num_of_samples = do
  -- create random generator
  rands <- randoms mtg
  let samples = mapP num_of_samples calcSample rands

  return [(key', samples)]
  where
  key' = case (td_PartValue . ae_TaskData $ env) of
    (Just n') -> mod key n'
    Nothing   -> key

{-
 The reduce function
-}
piReduce :: ReduceFunction Options K2 V3 V4
piReduce _env _opts _ v2s = return . Just . sum . map (B.count w1) $ v2s
