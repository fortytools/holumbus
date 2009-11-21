module Holumbus.MapReduce.Examples.MandelbrotSet.DMandel
(
   mandelMap
 , mandelReduce
)
where

import Holumbus.Distribution.SimpleDMapReduceIO
import Holumbus.MapReduce.Examples.MandelbrotSet.ImageTypes
import Holumbus.MapReduce.Examples.MandelbrotSet.ImageMandel
import Control.Parallel.Strategies

{- 

type MapFunction a k1 v1 k2 v2 = ActionEnvironment -> a -> k1 -> v1 -> IO [(k2, v2)]
-}
mandelMap :: MapFunction (Int, Int, Double, Int) Int [Int] Int [Lightness]
mandelMap _env (w,h,zmax,iter) y xs = do
  let xs' = parMap rnf (\x -> (y,[(gamma 4.0 . x') x])) xs 
  return xs'
  where
    x' x = imageMandel (Geo w h) zmax iter x y -- calc the value
    gamma g x = x ** (1/g)
  

{-

type ReduceFunction a k2 v2 v3 = ActionEnvironment -> a -> k2 -> [v2] -> IO (Maybe v3)
-}
mandelReduce :: ReduceFunction (Int, Int, Double, Int)  Int [Lightness] [Lightness]
mandelReduce _env _opts _k2 _v2s = undefined
