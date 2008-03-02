-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.MapReduce.Parallel
  Copyright  : Copyright (C) 2008 Sebastian M. Schlatt
  License    : MIT
  
  Maintainer : Sebastian M. Schlatt (sms@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  MapReduce with Local Parallelization

-}

-- TODO the map function should be a monadic io function, no arrow

-- ----------------------------------------------------------------------------
{-# OPTIONS -fscoped-type-variables -farrows #-}
-- ----------------------------------------------------------------------------

module Holumbus.Control.MapReduce.Parallel where

import Text.XML.HXT.Arrow

import           Holumbus.Control.Registry

import           Data.Map (Map,empty,insertWith,mapWithKey,filterWithKey)
import qualified Data.Map    as M
import           Data.Maybe (isJust, fromJust)

import           Control.Concurrent
import           Control.Monad

type Dict   = Map

-- ----------------------------------------------------------------------------

mapReduce :: (Ord k2) =>
                Int
             -> (k1 ->  v1  -> IO [(k2, v2)])
             -> (k2 -> [v2] -> IO (Maybe v3))
             -> [(k1, v1)]
             -> IO (M.Map k2 v3)
mapReduce maxWorkers mAP rEDUCE input
  = do
    ps  <- return (max 1 ((length input) `div` maxWorkers)) 
    runX (traceMsg 0 ("          MapReduce partition size: " ++ show ps))
    pin <- return ( partitionList ps input ) 

    runX (traceMsg 0 ("                    mapPerKey " ))
    mapped <- parallelMap mAP pin 
    
    runX (traceMsg 0 ("                    groupByKey " ))
    grouped <- return  (groupByKey mapped)  
    
    runX (traceMsg 0 ("                    reduceByKey "))
--    return M.empty
    reducePerKey rEDUCE grouped
    
-- ----------------------------------------------------------------------------

parallelMap :: (k1 ->  v1  -> IO [(k2, v2)]) -> [[(k1, v1)]] -> IO [(k2, v2)]
parallelMap mAP pl
  = do 
    result          <- newMVar []
    workers         <- startWorkers (runMapTask result mAP) pl
    waitForWorkers  workers
    res             <- takeMVar result
    return res


runMapTask :: MVar [(k2,v2)] -> (k1 ->  v1  -> IO [(k2, v2)]) -> [(k1, v1)] -> IO ()
runMapTask mv mAP input 
  = do
    mrMap   <- mapPerKey (uncurry mAP) input 
    current <- takeMVar mv
    combined <- return (mrMap ++ current)
    putMVar mv combined   
    
mapPerKey :: ((k1,v1)  -> IO [(k2, v2)]) -> [(k1, v1)] -> IO [(k2,v2)]
mapPerKey mAP []     = do return ([])
mapPerKey mAP (x:xs)
  = do
    the_x  <- mAP x
    the_xs <- mapPerKey mAP xs
    return $! (the_x ++ the_xs)
    -- maybe way nicer with mapM
    
reducePerKey :: (Ord k2) => (k2 -> [v2] -> IO(Maybe v3)) -> Map k2 [v2] -> IO (Map k2 v3)
reducePerKey rEDUCE inmap
  = rpk (uncurry rEDUCE) (M.toList inmap) M.empty
    where
      rpk :: (Ord k2) => ((k2, [v2]) -> IO(Maybe v3)) -> [(k2, [v2])] -> Map k2 v3 -> IO (Map k2 v3)
      rpk rEDUCE []      result = do return (result)
      rpk rEDUCE toProcess result
        = do
          first   <- return $ head toProcess
          applied <- rEDUCE first
          if isJust applied
            then rpk rEDUCE (drop 1 toProcess) (M.union result (M.singleton (fst first) (fromJust applied)))
            else rpk rEDUCE (drop 1 toProcess) result
          
           
       -- return M.empty
--    x <- mapWithKey rEDUCE inmap
    
--    x <- mapWithKey rEDUCE inmap
--      mapWithKey unJust            -- 3. Transform type to remove Maybe
--    . filterWithKey (const isJust) -- 2. Remove entries with value Nothing
--    . mapWithKey rEDUCE            -- 1. Apply *'\reduce'* per key
--   where 
--         unJust _ (Just v)   = v       -- Transforms optional into non-optional type
--         unJust _ Nothing    = error "guarded by filter"    

red :: a -> [b] -> IO (Maybe c)
red _ _ = do return Nothing

-- ----------------------------------------------------------------------------


mapReduce' :: (Ord k2) =>
                Int
             -> ((k1, v1) -> IOSLA (XIOState ()) XmlTree (k2, v2))
             -> (k2 -> [v2] -> Maybe v3)
             -> [(k1, v1)]
             -> IO (M.Map k2 v3)
mapReduce' maxWorkers mAP rEDUCE input 
  = do
       -- calculate partition size and partition input
    pl  <- return (max 1 ((length input) `div` maxWorkers)) 
    runX (traceMsg 0 ("          MapReduce partition size: " ++ show pl))
    pin <- return ( partitionList pl input ) 
    
    runX (traceMsg 0 ("                    mapPerKey " ))
    mall' <- parallelMap' mAP pin 
    
    runX (traceMsg 0 ("                    groupByKey " ))
    m'' <- return  (groupByKey mall')  
    
    runX (traceMsg 0 ("                    reduceByKey "))
    return (reducePerKey' rEDUCE m'')
    
-- ----------------------------------------------------------------------------

parallelMap' :: ((k1, v1) -> IOSLA (XIOState ()) XmlTree a) -> [[(k1, v1)]] -> IO [a]
parallelMap' mAP pl
  = do 
    result          <- newMVar []
    workers         <- startWorkers (runMapTask' result mAP) pl
    waitForWorkers  workers
    res             <- takeMVar result
    return res


runMapTask' :: MVar [c] -> ((k1, v1) -> IOSLA (XIOState ()) XmlTree c) -> [(k1, v1)] -> IO ()
runMapTask' mv mAP input 
  = do
    mrMap   <- runX (mapPerKey' mAP input) 
    current <- takeMVar mv
    combined <- return $! (head mrMap) ++ current
    putMVar mv combined        

-- ----------------------------------------------------------------------------


mapPerKey' :: (ArrowList a) => (a1 -> a b c) -> [a1] -> a b [c]
mapPerKey' mAP input = listA  $ catA $ 
      map mAP input
    
groupByKey :: (Ord k2) => [(k2, v2)] -> Dict k2 [v2]
groupByKey = foldl insert empty
  where
    insert dict (k2,v2) = insertWith (++) k2 [v2] dict

reducePerKey' :: (Ord k2) => (k2 -> v2 -> Maybe v3) -> Map k2 v2 -> Map k2 v3
reducePerKey' rEDUCE =
      mapWithKey unJust       -- 3. Transform type to remove Maybe
    . filterWithKey isItJust    -- 2. Remove entries with value Nothing
    . mapWithKey rEDUCE       -- 1. Apply *'\reduce'* per key
   where 
         isItJust = const isJust
--         isJust _ (Just _)   = True    -- Keep entries of this form
--         isJust _ Nothing    = False   -- Remove entries of this form
         unJust _ (Just v)   = v       -- Transforms optional into non-optional type
         unJust _ Nothing    = error "guarded by filter"

-- ----------------------------------------------------------------------------

-- | partition the list of input data into a list of input data lists of
--   approximately the same length
partitionList :: Int -> [a] -> [[a]]
partitionList _ [] = []
partitionList count l  = [take count l] ++ (partitionList count (drop count l)) 

-- ----------------------------------------------------------------------------

-- | reduce function to skip the reduce phase in a MapReduce computation
noReduce :: (Ord k2) => k2 -> [v2] -> Maybe v2 
noReduce _ = Just . head         
         