module Examples2.Mandel.DMandel
(
  dmandelAction
, dmandelActionMap
)
where

--import           Data.Binary

import           System.Log.Logger

--import qualified Holumbus.FileSystem.FileSystem as FS

import qualified Holumbus.Data.AccuMap as AMap
import qualified Holumbus.Data.KeyMap as KMap
import           Holumbus.MapReduce.Types
import           Examples2.Mandel.ImageTypes
import           Examples2.Mandel.ImageMandel


localLogger :: String
localLogger = "Examples2.Mandel.DMandel"


-- ----------------------------------------------------------------------------
-- Distributed-Map
-- ----------------------------------------------------------------------------


mapDMandel :: ActionEnvironment -> (Int, Int, Float, Int) -> Int -> [Int] -> IO [(Int, [Lightness])]
mapDMandel _ (w,h,zmax,iter) y xs
  = do 
    infoM localLogger "map DMandel"
    --debugM localLogger $ show ("input: " ++ show y ++ " - " ++ show xs)
    let xs' = map (\x -> (y,[(gamma 4.0 . x') x])) xs
    --debugM localLogger $ show $ "output: " ++ show xs'
    return xs'
    where
    x' x = imageMandel (Geo w h) zmax iter x y -- calc the value
    gamma g x = x ** (1/g)


reduceDMandel :: ActionEnvironment -> (Int, Int, Float, Int) -> Int -> [[Lightness]] -> IO (Maybe [Lightness])
reduceDMandel _ _ k vs 
  = do
    infoM localLogger "reduce/combine DMandel"
    --debugM localLogger $ show ("input: " ++ show k ++ " - " ++ show vs)
    let s = concat vs
    --debugM localLogger $ show $ "output: " ++ show s
    return (Just s)
    
  
partitionDMandel :: ActionEnvironment -> (Int, Int, Float, Int) -> Int -> [(Int, [Lightness])] -> IO [(Int,[(Int, [Lightness])])]
partitionDMandel _ _ _ ls 
  = do
    infoM localLogger "partition DMandel"
    --debugM localLogger $ show ls
    -- calculate partition-Values
--    let markedList = map (\t@(k,_) ->  (k,[t])) ls
    let markedList = map (\t@(k,_) ->  (k,t)) ls
    -- merge them
    let resultList = AMap.toList $ AMap.fromTupleList markedList
    return resultList


-- ----------------------------------------------------------------------------
-- Actions
-- ----------------------------------------------------------------------------

dmandelAction
  :: ActionConfiguration 
       (Int, Int, Float, Int)                      -- state
       Int [Int]                              -- k1, v1
       Int [Lightness]                              -- k2, v2
       [Lightness]                                     -- v3 == v2
       [Lightness]                                     -- v4
dmandelAction
  = (defaultActionConfiguration "DMANDEL")
        { ac_Map     = Just mapAction
        , ac_Combine = Nothing
        , ac_Reduce  = Just reduceAction
        }
    where
      mapAction 
        = (defaultMapConfiguration mapDMandel)
            { mc_Partition = partitionDMandel }
      reduceAction
        = (defaultReduceConfiguration reduceDMandel)
            { rc_Partition = partitionDMandel }

dmandelActionMap :: ActionMap
dmandelActionMap
  = KMap.insert (readActionConfiguration dmandelAction) $
    KMap.empty

