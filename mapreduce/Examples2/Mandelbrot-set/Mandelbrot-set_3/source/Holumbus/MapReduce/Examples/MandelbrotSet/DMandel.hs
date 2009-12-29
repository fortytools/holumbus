module Holumbus.MapReduce.Examples.MandelbrotSet.DMandel
(
   mandelMap
 , mandelReduce
)
where

import Holumbus.MapReduce.Examples.MandelbrotSet.SimpleDMapReduceIO
import Holumbus.MapReduce.Examples.MandelbrotSet.ImageTypes
import Holumbus.MapReduce.Examples.MandelbrotSet.ImageMandel

{- 

type MapFunction a k1 v1 k2 v2 = ActionEnvironment -> a -> k1 -> v1 -> IO [(k2, v2)]
-}
mandelMap :: MapF --unction (Int, Int, Double, Int) Int [(Int,[Int])] Int [(Int,[Lightness])]
mandelMap _env (w,h,zmax,iter) key values = do
--  let xs' = parMap rnf (\x -> (y,[(gamma 4.0 . x') x])) xs 
  let result = map (\(y,xs) -> (y,map (gamma 4.0 . x'  y) xs)) values
  return [(key,result)]
  where
    x' y x= imageMandel (Geo w h) zmax iter x y -- calc the value
    gamma g x = x ** (1/g)
  

{-

type ReduceFunction a k2 v2 v3 = ActionEnvironment -> a -> k2 -> [v2] -> IO (Maybe v3)
-}
mandelReduce :: ReduceF --unction (Int, Int, Double, Int)  Int [(Int,[Lightness])] [(Int,[Lightness])]
mandelReduce = undefined
