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

import           Holumbus.Utility
import           Holumbus.Control.MapReduce.MapReducible

-- import           System.Time

type Dict   = Map


-- ----------------------------------------------------------------------------

-- | MapReduce Computations
mapReduce :: (Ord k2 , MapReducible mr k2 v2, Show k2) =>
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
    runX (traceMsg 0 ("                    reducePerKey " ))
    reducePerKey maxWorkers mr grouped
    
-- ----------------------------------------------------------------------------

-- | Executes the map phase of a MapReduce computation as a parallel computation.
parallelMap :: Int -> (k1 ->  v1  -> IO [(k2, v2)]) -> [(k1, v1)] -> IO [(k2, v2)]
parallelMap maxWorkers mapFunction inputData
  = if (length inputData > 0) 
      then do
           chan <- newChan
           parallelMap' chan 0 maxWorkers mapFunction inputData []
      else return []

parallelMap' :: Chan [(k2, v2)] -> Int -> Int -> (k1 ->  v1  -> IO [(k2, v2)]) 
             -> [(k1, v1)] -> [(k2, v2)] -> IO [(k2, v2)]
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
  = forkIO ( do
             res <- catch (mapFunction k1 v1) (\_ -> return $ [])
             writeChan chan res
             return ()
           ) >> return ()

-- ----------------------------------------------------------------------------

-- | Groups the output data of the Map phase by the computed keys (k2) 
groupByKey :: (Ord k2) => [(k2, v2)] -> Dict k2 [v2]
groupByKey = foldl insert empty
  where
    insert dict (k2,v2) = insertWith (++) k2 [v2] dict

-- ----------------------------------------------------------------------------

{- not in use

-- | Reads debugging information from Chan and display it. Makes debug information readable even
--   when multiple threads have something to say
showLogs :: Chan (String) -> IO ()
showLogs logChan = do
                   s <- readChan logChan
--                   putStrLn s
                   showLogs logChan
-}

-- | Parallel key-wise reduction
reducePerKey :: (MapReducible mr k2 v2, Show k2) => Int -> mr -> Map k2 [v2] -> IO (mr)
reducePerKey maxWorkers initialMR m = 
  let input   = partitionListByCount maxWorkers (M.toList m)           -- partition input data
      workers = length input                                           -- get real worker count
  in
  do
  resChan <- newChan                                 -- channel where threads will put their results
  logChan <- newChan                                 -- channel for log messages
--  forkIO (showLogs logChan)                          -- start log output thread
  spawnThreads logChan resChan (zip [1..] input)     -- spawn worker threads
  foldM (\mr (_c,_a) -> do                             -- get all results from result channel
                      res <- readChan resChan        
--                      clt <- getClockTime            
--                      cat <- toCalendarTime clt      -- get and trace time for debugging
--                      writeChan logChan ("--  " ++ (calendarTimeToString cat) ++ " processing result " ++ show c ++ " / " ++ show a)
                      if isJust res then mergeMR mr (fromJust res) else return mr
        ) initialMR ( zip [1..workers] (repeat workers) )
  where
    spawnThreads _ _ [] = return ()         -- no more threads to spawn
    spawnThreads logChan resChan l  = do    -- spawn new thread and make recursive call
                                      runReduceTask logChan resChan initialMR (head l)
                                      spawnThreads logChan resChan (tail l)


-- | Run one thread as part of the parallel map phase
runReduceTask :: (MapReducible mr k2 v2, Show k2) => 
                 Chan (String) -> Chan (Maybe mr) ->  mr -> (Int, [(k2, [v2])]) -> IO ()
runReduceTask logChan chan initialMR (threadId, input)
  = forkIO                                                                -- spawn new thread
    ( do                                                    
      res <- catch                                                        -- catch exception so the
             ( do                                                         -- MapReduce can continue
               r <- foldM (  \mr (k2, v2s)                               -- if one worker fails
                           -> do
                              writeChan logChan ("--    rEDUCE: thread " ++ show threadId ++ " processing " ++ show k2)
                              val   <- reduceMR initialMR k2 v2s           -- apply rEDUCE function
                              if isJust val                                -- process result
                                 then mergeMR mr (fromJust val)
                                 else return mr
                          ) initialMR input
               return $! Just r
             ) 
             (\_ -> return Nothing)  
      writeChan chan res                                                  -- write result to channel  
      return ()
    ) >> return ()

 
