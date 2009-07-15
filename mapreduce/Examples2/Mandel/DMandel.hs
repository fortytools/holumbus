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
import           Control.Parallel.Strategies

localLogger :: String
localLogger = "Examples2.Mandel.DMandel"


-- ----------------------------------------------------------------------------
-- Distributed-Map
-- ----------------------------------------------------------------------------


mapDMandel :: ActionEnvironment -> (Int, Int, Double, Int) -> Int -> [Int] -> IO [(Int, [Lightness])]
mapDMandel _ (w,h,zmax,iter) y xs
  = do 
    infoM localLogger "map DMandel"
    --debugM localLogger $ show ("input: " ++ show y ++ " - " ++ show xs)
    let xs' = {-# SCC "dmandel1" #-}map (\x -> (y,[(gamma 4.0 . x') x])) xs
    --debugM localLogger $ show $ "output: " ++ show xs'
    return xs'
    where
    x' x = {-# SCC "dmandel2" #-}imageMandel (Geo w h) zmax iter x y -- calc the value
    gamma g x ={-# SCC "dmandel3" #-} x ** (1/g)


reduceDMandel :: ActionEnvironment -> (Int, Int, Double, Int) -> Int -> [[Lightness]] -> IO (Maybe [Lightness])
reduceDMandel _ _ k vs 
  = do
    infoM localLogger "reduce/combine DMandel"
    --debugM localLogger $ show ("input: " ++ show k ++ " - " ++ show vs)
    let s = {-# SCC "dmandel4" #-}concat vs
    --debugM localLogger $ show $ "output: " ++ show s
    {-# SCC "dmandel5" #-} return (Just s)

-- ----------------------------------------------------------------------------
-- Actions
-- ----------------------------------------------------------------------------

dmandelAction
  :: ActionConfiguration 
       (Int, Int, Double, Int)                      -- state
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
            { mc_Partition = hashedPartition }
      reduceAction
        = (defaultReduceConfiguration reduceDMandel)
            { rc_Partition = hashedPartition }

dmandelActionMap :: ActionMap
dmandelActionMap
  = KMap.insert (readActionConfiguration dmandelAction) $
    KMap.empty

