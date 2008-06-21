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

-- import           Data.Map (Map) -- ,mapWithKey,filterWithKey)
-- import qualified Data.Map    as M
import           Data.Maybe (isJust, fromJust)
import           Data.Binary
import qualified Data.Set as S

import           Control.Concurrent
import           Control.Monad

import           Holumbus.Control.MapReduce.MapReducible
import           Holumbus.Utility
import           Holumbus.Index.Common

-- ----------------------------------------------------------------------------

-- | MapReduce Computations
mapReduce :: (Show k2, Binary v2, Ord k2 , MapReducible mr k2 v2) =>
                FilePath
             -> Int                             -- ^ No. of Threads 
             -> mr                              -- ^ initial value for the result
             -> (k1 ->  v1  -> IO [(k2, v2)])   -- ^ Map function
             -> [(k1, v1)]                      -- ^ input data 
             -> IO (mr)  
mapReduce path maxWorkers mr mapFunction input
  = do
    
    -- parallel map phase
    runX (traceMsg 0 ("                    mapPerKey " ))
    mapped <- parallelMap path maxWorkers mapFunction input  
    
    -- reduce phase
    runX (traceMsg 0 ("                    reduceByKey "))
    reducePerKey path mr mapped
    return mr
-- ----------------------------------------------------------------------------

-- | Executes the map phase of a MapReduce computation as a parallel computation.
parallelMap :: (Ord k2, Binary v2, Show k2) => FilePath -> Int -> (k1 ->  v1  -> IO [(k2, v2)]) -> [(k1, v1)] -> IO (S.Set k2)
parallelMap path maxWorkers mapFunction inputData
  = if (length inputData > 0) 
      then do
           chan <- newChan
           parallelMap' path chan 0 maxWorkers mapFunction inputData S.empty
      else return S.empty

parallelMap' :: (Ord k2, Show k2, Binary v2) => FilePath -> Chan [(k2, v2)] -> Int -> Int -> (k1 ->  v1  -> IO [(k2, v2)]) 
            -> [(k1, v1)] -> S.Set k2 -> IO (S.Set k2)
parallelMap' path chan activeWorkers maxWorkers mapFunction inputData result
  = do
    if (activeWorkers < maxWorkers) && ((length inputData) > 0)
      then do
           runMapTask chan mapFunction (head inputData)
           parallelMap' path chan (activeWorkers + 1) maxWorkers mapFunction (tail inputData) result
      else if (activeWorkers == 0) && (length inputData == 0)
             then return result
             else do
                  yield --TODO necessary?
                  readValues activeWorkers result
  where
  readValues workers theResult = do
                  pairs  <- readChan chan  
                  keys   <- foldM appendResult S.empty pairs
                  e      <- isEmptyChan chan
                  if e
                    then parallelMap' path chan (workers - 1) maxWorkers mapFunction inputData (S.union theResult keys)
                    else readValues (workers -1) (S.union theResult keys)
--  appendResult :: (Show k2, Binary k2) => (k2, v2) -> IO k2
  appendResult s (k,v) = let thePath = (path ++ fileName k)
                         in if S.member k s 
                              then do
                                   vs <- strictDecodeFile thePath
                                   writeToBinFile thePath (v : vs)
                                   return $ S.union s (S.singleton k)
                              else do 
                                   writeToBinFile thePath [v]
                                   return $ S.union s (S.singleton k)

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
{-
groupByKey :: (Ord k2) => S.Set k2 -> Dict k2 [v2]
groupByKey = foldl insert empty
  where
    insert dict (k2,v2) = insertWith (++) k2 [v2] dict
-}

-- | Reduce Phase. The Reduce Phase does not run parallelized.
reducePerKey :: (MapReducible mr k2 v2, Binary v2, Show k2) => FilePath -> mr -> S.Set k2 -> IO (mr)
reducePerKey path initialMR ks = foldM (rpk (reduceMR initialMR)) initialMR (S.toList ks)
   where
   rpk :: (MapReducible mr k2 v2, Binary v2, Show k2) => (k2 -> [v2] -> IO(Maybe mr)) -> mr -> k2 -> IO(mr)
   rpk reduceFunction initial key 
     = do 
       values <- strictDecodeFile (path ++ fileName key)
       new    <- reduceFunction key values 
       if isJust new
           then return (mergeMR initial (fromJust new))
           else return initial    

fileName :: Show k2 => k2 -> String
fileName key = stripWith (== '"') (show key) ++ ".bin"
           