module Holumbus.MapReduce.Examples.MandelbrotSet.DMandel
(
   mapF
 , reduceF
 , splitF 
 , sortImage
)
where

-- Holumbus imports
import Holumbus.MapReduce.Types

-- Mandelbrot imports
import Holumbus.MapReduce.Examples.MandelbrotSet.SimpleDMapReduceIO
import Holumbus.MapReduce.Examples.MandelbrotSet.ImageTypes hiding (Image)
import Holumbus.MapReduce.Examples.MandelbrotSet.ImageMandel

-- System import
import Control.Parallel.Strategies
import Data.Binary
import Data.List
import Control.Parallel.Strategies
import System.Log.Logger
import Control.Exception
import Control.Parallel.Strategies

{- ------------------------------------------------------------------------------------------------------------- -}

localLogger = "Holumbus.MapReduce.Examples2.DMandel"
 
{- ------------------------------------------------------------------------------------------------------------ -}
{-

the split partition
type SplitF  = SplitFunction A BlockID (Image XCoord)
type SplitPartition a k1 v1 = ActionEnvironment -> a -> k1 -> v1 -> IO [(Int, [(k1,v1)])]


K1 = the blockid which represents the block created by the client.
V1 = the image with their associated split block id which is the same as the key
l = [(k1,v1)] = [(BlockId, Image XCoord)] = [(BlockId, [(YCoord, [a])])]

[(Int,[(BlockId, [(YCoord, [a])])])]

-}
splitF :: SplitF
splitF _env (split,_,_,_,_) _n l = do
  m <- mapM (splitF' split) l
  let t= concat m in rnf t `seq` return t

splitF' :: Int -> (K1,V1) -> IO [(Int, [(K1,V1)])]
splitF' split (blockid,image) = do
  debugM localLogger $ "length:  " ++ show len
  debugM localLogger $ "split: " ++ show split
  debugM localLogger $ "list':  " ++ show l'  
  return l'
  where
  l' = p image -- zipWith f [(blockid*10)..] (p image)
--  f i image = (i,[(blockid,image)])
  len = (length image)
  p image = part2 blockid split len image
  
{-part2 :: Int -> Int -> [a] -> [[a]]
part2 parts len images = part' parts (len `div` parts) 1 images
  where
  part' :: Int -> Int -> Int -> [a] -> [[a]]
  part' parts size i l
    | i == parts = [l]
    | otherwise  = (fst : part' parts size i' rst)
    where
    (fst,rst) = splitAt size l
    i' = i + 1
-}
  
part2 :: K1 -> Int -> Int -> V1 -> [(K1,[(K1,V1)])]
part2 key parts len images = part2' key parts (len `div` parts) (1+10*key) images
  where
  part2' :: K1 -> Int -> Int -> K1 -> V1 -> [(K1,[(K1,V1)])]
  part2' key parts size i l
    | i == (parts+10*key) = [(i, [(key,l)])]
    | otherwise  = ((i, [(key,fst)]) : part2' key parts size i' rst)
    where
    (fst,rst) = splitAt size l
    i' = i + 1

  
{- 

type MapFunction a k1 v1 k2 v2 = ActionEnvironment -> a -> k1 -> v1 -> IO [(k2, v2)]

  -- do the lightness computing
  -- set the splitblockid as key
  -- return the image lightNess
  
-}
mapF :: MapF -- unction Options BlockID (SplitImage XCoord) BlockID (BlockID, BlockID, Image Lightness)
mapF env (_, w,h,zmax,iter) key image  = do
  debugM localLogger $ "Map Key:" ++ show key
  debugM localLogger $ "Map Key:" ++ show image
  return [(key, image')]
    where
    image' :: Image Lightness
    image' = map (\(y,xs) -> (y,map (gamma 4.0 . x' y) xs)) image
  
    x' :: YCoord -> XCoord -> Lightness
    x' y x = imageMandel (Geo w h) zmax iter x y -- calc the value

    gamma :: Double -> Lightness -> Lightness
    gamma g x = x ** (1/g)

{-

type ReduceFunction a k2 v2 v3 = ActionEnvironment -> a -> k2 -> [v2] -> IO (Maybe v3)
-}
reduceF :: ReduceF --unction Options BlockID (BlockID, Image Lightness) (Image Lightness)
reduceF _env _opts key images = do
  debugM localLogger $ "Reduce Key:" ++ show key
  debugM localLogger $ "sorted:" ++ show sorted
  debugM localLogger $ "concated:" ++ show concated  
  debugM localLogger $ "images:" ++ show images
  
  return . Just $sorted
  where
  concated = concat images
  sorted = sortBy sortImage concated

{-
  order function the pixels 
-}
sortImage :: (Ord k) => (k,v) -> (k,v) -> Ordering
sortImage (k1,_) (k2,_) 
  | k1 > k2 = GT
  | k1 < k2 = LT
  | otherwise = EQ
  
