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
import           Holumbus.Control.MapReduce.MapReducible
import           Holumbus.Utility

import           Data.Map (Map,empty,insertWith) -- ,mapWithKey,filterWithKey)
import qualified Data.Map    as M
import           Data.Maybe (isJust, fromJust)

import           Control.Concurrent
import           Control.Monad

type Dict   = Map


-- ----------------------------------------------------------------------------

-- | MapReduce Computations
{- mapReduce :: (Ord k2 , MapReduceable mr k2 v2 v3) =>
                Int                             -- ^ No. of Threads 
             -> mr                              -- ^ initial value for the result
             -> (k1 ->  v1  -> IO [(k2, v2)])   -- ^ Map function
             -> (k2 -> [v2] -> IO (Maybe v3))   -- ^ Reduce function
             -> [(k1, v1)]                      -- ^ input data 
             -> IO (mr)                -- ^ Result is a Map
-}
mapReduce maxWorkers mr mapFunction input
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
    reducePerKey mr grouped
    
-- ----------------------------------------------------------------------------

-- | Executes the map phase of a MapReduce computation as a parallel computation.
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

-- | Runs a map task in a thread. the reduce function function is wrapped into
--   everything that is necessary to return the results. The input data will be
--   a partition of the input data of the whole MapReduce Computation
runMapTask :: MVar [(k2,v2)] -> (k1 ->  v1  -> IO [(k2, v2)]) -> [(k1, v1)] -> IO ()
runMapTask mv mapFunction input 
  = do
    mrMap   <- mapPerKey (uncurry mapFunction) input 
    current <- takeMVar mv
    combined <- return (mrMap ++ current)
    putMVar mv combined   
    
-- ----------------------------------------------------------------------------

-- | Applies the map function to every data in the input list
mapPerKey :: ((k1,v1)  -> IO [(k2, v2)]) -> [(k1, v1)] -> IO [(k2,v2)]
mapPerKey _ []     = do return ([])
mapPerKey mapFunction (x:xs)
  = do
    the_x  <- mapFunction x
    the_xs <- mapPerKey mapFunction xs
    return $! (the_x ++ the_xs)
    -- maybe way nicer with mapM
    
-- | Groups the output data of the Map phase by the computed keys (k2) 
groupByKey :: (Ord k2) => [(k2, v2)] -> Dict k2 [v2]
groupByKey = foldl insert empty
  where
    insert dict (k2,v2) = insertWith (++) k2 [v2] dict


-- reducePerKey :: (MapReduceable mr k2 v2 v3) => mr -> Map k2 [v2] -> IO (mr)
reducePerKey initialMR m
 = rpk 
    (uncurry (reduceMR initialMR)) 
    (M.toList m) 
    initialMR 
   where
--     rpk :: (MapReduceable mr k2 v2 v3) => ((k2, [v2]) -> IO(Maybe mr)) -> [(k2, [v2])] -> mr -> IO(mr)
--     rpk _ _ r = return r
     rpk _  [] result = return result
     rpk rf l  result
       = do 
         next <- return $ head l
         done <- rf next
         if isJust done
           then rpk rf (drop 1 l) (mergeMR result (fromJust done))
           else rpk rf (drop 1 l) result 
     
-- | Reduce Phase. The Reduce Phase does not run parallelized.
redPerKey :: (Ord k2) => (k2 -> [v2] -> IO(Maybe v3)) -> Map k2 [v2] -> IO (Map k2 v3)
redPerKey reduceFunction m
  = rpk (uncurry reduceFunction) (M.toList m) M.empty
    where
      rpk :: (Ord k2) => ((k2, [v2]) -> IO(Maybe v3)) -> [(k2, [v2])] -> Map k2 v3 -> IO (Map k2 v3)
      rpk _ [] result = return result
      rpk reduceFunction' toProcess result
        = do
          firstP  <- return $ head toProcess
          applied <- reduceFunction' firstP
          if isJust applied
            then rpk reduceFunction' (drop 1 toProcess) (M.union result (M.singleton (fst firstP) (fromJust applied)))
            else rpk reduceFunction' (drop 1 toProcess) result
          

-- ----------------------------------------------------------------------------
 

         