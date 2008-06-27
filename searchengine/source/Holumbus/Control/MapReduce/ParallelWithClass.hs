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

module Holumbus.Control.MapReduce.ParallelWithClass 
  (
    mapReduce
  ) 
where

import Text.XML.HXT.Arrow

import           Data.Map (Map,empty,insertWith) -- ,mapWithKey,filterWithKey)
import qualified Data.Map    as M
import           Data.Maybe (isJust, fromJust)
-- import           Data.Binary

import           Control.Concurrent
import           Control.Monad

import           Holumbus.Control.MapReduce.MapReducible

type Dict   = Map


-- ----------------------------------------------------------------------------

-- | MapReduce Computations
mapReduce :: (Ord k2 , MapReducible mr k2 v2) =>
                Int                             -- ^ No. of Threads 
             -> mr                              -- ^ initial value for the result
             -> (k1 ->  v1  -> IO [(k2, v2)])   -- ^ Map function
             -> [(k1, v1)]                      -- ^ input data 
             -> IO (mr)  
mapReduce maxWorkers mr mapFunction input
  = do
    
    -- parallel map phase
    runX (traceMsg 0 ("                    mapPerKey " ))
    mapped <- parallelMap maxWorkers mapFunction input
    
    -- grouping of data gained in the map phase
    runX (traceMsg 0 ("                    groupByKey " ))
    grouped  <- return  (groupByKey mapped)  
    
    -- reduce phase
    runX (traceMsg 0 ("                    reduceByKey "))
    reducePerKey mr grouped
    
-- ----------------------------------------------------------------------------

-- | Executes the map phase of a MapReduce computation as a parallel computation.
parallelMap :: Int -> (k1 ->  v1  -> IO [(k2, v2)]) -> [(k1, v1)] -> IO [(k2, v2)]
parallelMap maxWorkers mapFunction inputData
  = if (length inputData > 0) 
      then do
           chan <- newChan
           parallelMap' chan 0 maxWorkers mapFunction inputData []
      else return []

parallelMap' :: Chan [(k2, v2)] -> Int -> Int -> (k1 ->  v1  -> IO [(k2, v2)]) -> [(k1, v1)] -> [(k2, v2)] -> IO [(k2, v2)]
parallelMap' chan activeWorkers maxWorkers mapFunction inputData result
  = do
    if (activeWorkers < maxWorkers) && ((length inputData) > 0)
      then do
           runMapTask chan mapFunction (head inputData)
           parallelMap' chan (activeWorkers + 1) maxWorkers mapFunction (tail inputData) result
      else if (activeWorkers == 0) -- && (length inputData == 0)
             then return result
             else do
                  yield
                  readValues activeWorkers result
  where
  readValues workers theResult = do
                  res   <- readChan chan  
                  e     <- isEmptyChan chan
                  if e
                    then parallelMap' chan (workers - 1) maxWorkers mapFunction inputData (res ++ theResult)
                    else readValues (workers -1) (res ++ theResult)

runMapTask :: Chan [(k2, v2)] -> (k1 ->  v1  -> IO [(k2, v2)]) -> (k1, v1) -> IO ()
runMapTask chan mapFunction (k1, v1)
  = do 
    forkIO ( do
             res <- catch (mapFunction k1 v1) (\_ -> return $ [])
             writeChan chan res
             return ()
           )
    return ()

-- ----------------------------------------------------------------------------
{-
-- | Applies the map function to every data in the input list
mapPerKey :: ((k1,v1)  -> IO [(k2, v2)]) -> [(k1, v1)] -> IO [(k2,v2)]
mapPerKey _ []     = do return ([])
mapPerKey mapFunction (x:xs)
  = do
    the_x  <- mapFunction x
    the_xs <- mapPerKey mapFunction xs
    return $! (the_x ++ the_xs)
    -- maybe way nicer with mapM
-}    
    
-- | Groups the output data of the Map phase by the computed keys (k2) 
groupByKey :: (Ord k2) => [(k2, v2)] -> Dict k2 [v2]
groupByKey = foldl insert empty
  where
    insert dict (k2,v2) = insertWith (++) k2 [v2] dict

-- | Reduce Phase. The Reduce Phase does not run parallelized.
reducePerKey :: (MapReducible mr k2 v2) => mr -> Map k2 [v2] -> IO (mr)
reducePerKey initialMR m
 = rpk 
    (uncurry (reduceMR initialMR)) 
    (M.toList m) 
    initialMR 
   where
     rpk :: (MapReducible mr k2 v2) => ((k2, [v2]) -> IO(Maybe mr)) -> [(k2, [v2])] -> mr -> IO(mr)
     rpk _  [] result = return result
     rpk rf l  result
       = do 
         next <- return $ head l
         done <- rf next
         if isJust done
           then rpk rf (drop 1 l) (mergeMR result (fromJust done))
           else rpk rf (drop 1 l) result    
