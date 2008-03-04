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

import           Data.Map (Map,empty,insertWith) -- ,mapWithKey,filterWithKey)
import qualified Data.Map    as M
import           Data.Maybe (isJust, fromJust)

import           Control.Concurrent
import           Control.Monad

type Dict   = Map

-- ----------------------------------------------------------------------------

-- | MapReduce Computations
mapReduce :: (Ord k2) =>
                Int
             -> (k1 ->  v1  -> IO [(k2, v2)])
             -> (k2 -> [v2] -> IO (Maybe v3))
             -> [(k1, v1)]
             -> IO (M.Map k2 v3)
mapReduce maxWorkers mapFunction reduceFunction input
  = do
    
    -- split the input data into pieces of approximately the same size
    ps       <- return (max 1 ((length input) `div` maxWorkers)) 
    runX (traceMsg 0 ("          MapReduce partition size: " ++ show ps))
    pin      <- return ( partitionList ps input ) 

    -- parallel map phase
    runX (traceMsg 0 ("                    mapPerKey " ))
    mapped   <- parallelMap mapFunction pin 
    
    -- grouping of data gained in the map phase
    runX (traceMsg 0 ("                    groupByKey " ))
    grouped  <- return  (groupByKey mapped)  
    
    -- reduce phase
    runX (traceMsg 0 ("                    reduceByKey "))
    reducePerKey reduceFunction grouped
    
-- ----------------------------------------------------------------------------

-- | executes the map phase of a MapReduce computation as a parallel computation.
--   it is not very smart so far since the input data is split in the beginning
--   and there is no work load balancing during the computation
parallelMap :: (k1 ->  v1  -> IO [(k2, v2)]) -> [[(k1, v1)]] -> IO [(k2, v2)]
parallelMap mapFunction pl
  = do 
    result          <- newMVar []
    workers         <- startWorkers (runMapTask result mapFunction) pl
    waitForWorkers  workers
    res             <- takeMVar result
    return res


runMapTask :: MVar [(k2,v2)] -> (k1 ->  v1  -> IO [(k2, v2)]) -> [(k1, v1)] -> IO ()
runMapTask mv mapFunction input 
  = do
    mrMap   <- mapPerKey (uncurry mapFunction) input 
    current <- takeMVar mv
    combined <- return (mrMap ++ current)
    putMVar mv combined   
    
-- ----------------------------------------------------------------------------

mapPerKey :: ((k1,v1)  -> IO [(k2, v2)]) -> [(k1, v1)] -> IO [(k2,v2)]
mapPerKey _ []     = do return ([])
mapPerKey mapFunction (x:xs)
  = do
    the_x  <- mapFunction x
    the_xs <- mapPerKey mapFunction xs
    return $! (the_x ++ the_xs)
    -- maybe way nicer with mapM
    
groupByKey :: (Ord k2) => [(k2, v2)] -> Dict k2 [v2]
groupByKey = foldl insert empty
  where
    insert dict (k2,v2) = insertWith (++) k2 [v2] dict
    
reducePerKey :: (Ord k2) => (k2 -> [v2] -> IO(Maybe v3)) -> Map k2 [v2] -> IO (Map k2 v3)
reducePerKey reduceFunction inmap
  = rpk (uncurry reduceFunction) (M.toList inmap) M.empty
    where
      rpk :: (Ord k2) => ((k2, [v2]) -> IO(Maybe v3)) -> [(k2, [v2])] -> Map k2 v3 -> IO (Map k2 v3)
      rpk _ [] result = do return (result)
      rpk reduceFunction' toProcess result
        = do
          firstP  <- return $ head toProcess
          applied <- reduceFunction' firstP
          if isJust applied
            then rpk reduceFunction' (drop 1 toProcess) (M.union result (M.singleton (fst firstP) (fromJust applied)))
            else rpk reduceFunction' (drop 1 toProcess) result
          

-- ----------------------------------------------------------------------------
    
-- | partition the list of input data into a list of input data lists of
--   approximately the same length
partitionList :: Int -> [a] -> [[a]]
partitionList _ [] = []
partitionList count l  = [take count l] ++ (partitionList count (drop count l)) 

         