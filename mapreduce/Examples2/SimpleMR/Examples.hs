module MyExamples.SimpleMR.Examples
where

import qualified Holumbus.Data.AccuMap as AMap
import qualified Holumbus.Data.KeyMap as KMap

import MyExamples.SimpleMR.SimpleMR
import MyExamples.Mandel.DMandel
--import MyExamples.Mandel.ImageTypes
import MyExamples.Mandel.ImageMandel

-- a  :: (Int,Int,Float,Int)
-- k1 :: Int
-- v1 :: [Int]
-- k2 :: Int
-- v2 :: [Lightness]
-- v3 :: [Lightness]

type Lightness = Int

--          a -> k1 -> v1 -> IO [(k2, v2)]
mapf :: Map (Int,Int,Float,Int) Int [Int] Int [Lightness]
mapf (h,w,zmax,iter) y xs = do 
--  let res = map (\x -> (y,[imageMandel (Geo w h) zmax iter x y])) xs
  let res = map (\x -> (y,[x])) xs
  return res

-- a -> k2 -> [v2] -> IO (Maybe v3)
reducef :: Reduce (Int,Int,Float,Int) Int [Lightness] [Lightness]
reducef _ _ ls = do
  let res = concat ls
  return (Just res)
  
-- a -> Int -> [(k2,v2)] -> IO [(Int, [(k2,v2)])]
part_map :: MapPartition (Int,Int,Float,Int) Int [Lightness]
part_map _ _ ls = do
  let res = map (\t@(k,_) ->  (k,[t])) ls
--  let res = AMap.toList $ AMap.fromTupleList markedList
  return res

-- a -> Int -> [(k2,v3)] -> IO [(Int, [(k2,v3)])]
part_reduce :: ReducePartition (Int,Int,Float,Int) Int [Lightness]
part_reduce _ _ ls = do
  let markedList = map (\t@(k,_) ->  (k,t)) ls
  let res = AMap.toList $ AMap.fromTupleList markedList
  return res
  
p :: (Int, [(Int,[Int])])
p = (1,pixels 10 10)
  
pixels :: Int -> Int -> [(Int,[Int])]
pixels w h
  = [(y,take w [0..])|y<-[0..h-1]] 