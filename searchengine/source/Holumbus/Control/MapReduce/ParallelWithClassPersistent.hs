-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.MapReduce.Parallel
  Copyright  : Copyright (C) 2008 Sebastian M. Schlatt
  License    : MIT
  
  Maintainer : Sebastian M. Schlatt (sms@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  MapReduce with Local Parallelization and temporary data stored in a SQLite DB

-}

-- ----------------------------------------------------------------------------
{-# OPTIONS -fscoped-type-variables -farrows #-}
-- ----------------------------------------------------------------------------

module Holumbus.Control.MapReduce.ParallelWithClassPersistent 
  (
    mapReduce
  ) 
where

import Text.XML.HXT.Arrow

import           Data.Map (Map,empty) -- ,mapWithKey,filterWithKey)
import qualified Data.Map    as M
import           Data.Maybe (isJust, fromJust)
import           Data.Binary

import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL -- hiding (head, map, foldl)

import           Control.Concurrent
import           Control.Monad

import           Holumbus.Control.MapReduce.MapReducible
import           Holumbus.Utility

import           Database.HDBC
import           Database.HDBC.Sqlite3

-- ----------------------------------------------------------------------------

-- | MapReduce Computations
mapReduce :: (Ord k2 , Binary v2, MapReducible mr k2 v2) =>
                Int                             -- ^ No. of Threads 
             -> FilePath                        -- ^ Path for SQLite database
             -> mr                              -- ^ initial value for the result
             -> (k1 ->  v1  -> IO [(k2, v2)])   -- ^ Map function
             -> [(k1, v1)]                      -- ^ input data 
             -> IO (mr)  
mapReduce maxWorkers sqlitePath mr mapFunction input
  = do
    
    conn <- createConnection sqlitePath                        -- create Sqlite Connection
    
                                                                
    runX (traceMsg 0 ("                    mapPerKey " ))      
    mapped <- parallelMap conn maxWorkers  mapFunction input   -- parallel mAP phase
    
    -- reduce phase
    runX (traceMsg 0 ("                    reduceByKey "))     
    reducePerKey conn maxWorkers mr mapped                     -- parallel rEDUCE phase
    
-- ----------------------------------------------------------------------------

-- | Executes the map phase of a MapReduce computation as a parallel computation.
parallelMap :: (Ord k2, Binary v2) => 
     Connection                    -- ^ SQLite database connection 
  -> Int                           -- ^ maximum number of threads
  -> (k1 ->  v1  -> IO [(k2, v2)]) -- ^ mAP function
  -> [(k1, v1)]                    -- ^ input data
  -> IO (Map k2 Int)               -- ^ dictionary mapping keys to database identifiers
parallelMap conn maxWorkers  mapFunction inputData
  = if (length inputData > 0)   -- start recursive function only if input data is supplied
      then do
           chan <- newChan      -- open communication channel and call recursive helper function
           parallelMap' chan conn 0 maxWorkers mapFunction 1 inputData M.empty
      else return M.empty

-- | Recursive helper function for the parallel mAP phase
parallelMap' :: (Ord k2, Binary v2) => 
     Chan [(k2, v2)]               -- ^ communication channel for main-thread and workers  
  -> Connection                    -- ^ SQLite database connection
  -> Int                           -- ^ active worker-thread count
  -> Int                           -- ^ maximum number of workers 
  -> (k1 ->  v1  -> IO [(k2, v2)]) -- ^ mAP function
  -> Int                           -- ^ next database id to use
  -> [(k1, v1)]                    -- ^ input data 
  -> Map k2 Int                    -- ^ accumulator for the result 
  -> IO (Map k2 Int)               -- ^ dictionary mapping keys to database identifiers
parallelMap' chan conn activeWorkers maxWorkers mapFunction nextId inputData acc
  = do
    if (activeWorkers < maxWorkers) && ((length inputData) > 0)    -- spawn new workers ?     
      then do
           runMapTask chan mapFunction (head inputData)            -- start a new worker
           parallelMap' chan conn (activeWorkers + 1) maxWorkers   -- recursive call
                        mapFunction nextId (tail inputData) acc
      else if (activeWorkers == 0) -- && (length inputData == 0)   -- mAP phase finished ?
             then return acc                                       -- if yes return result
             else do                                              
                  yield                                            -- maybe redundant
                  readValues activeWorkers nextId acc              -- process results from workers
  where
  readValues workers nextId' acc' = do
                  res   <- readChan chan                           -- wait for first results in chan
                  (n, a) <- foldM processValue (nextId', acc') res -- process results from 1st worker
                  e     <- isEmptyChan chan                        -- more results in chan?
                  if e
                    then parallelMap' chan conn (workers - 1) maxWorkers mapFunction n inputData (a)
                    else readValues (workers -1) n a
  processValue :: (Ord k2, Binary v2) => (Int, Map k2 Int) -> (k2, v2) -> IO (Int, Map k2 Int)
  processValue (next, m) (k2, v2) = 
    do
    let k2Id = M.findWithDefault next k2 m                         -- get Id for database entry
    storeValue conn k2Id v2                                        -- store data in SQLite db
    return $ (if next == k2Id then next + 1 else next, M.insert k2 k2Id m) 


runMapTask :: Chan [(k2, v2)] -> (k1 ->  v1  -> IO [(k2, v2)]) -> (k1, v1) -> IO ()
runMapTask chan mapFunction (k1, v1)                                  -- start new Thread
  = do forkIO ( do                                                    
                res <- catch (mapFunction k1 v1) (\_ -> return $ [])  -- apply mAP
                writeChan chan res                                    -- write result to channel  
                return ()
              )
       return ()

-- ----------------------------------------------------------------------------

reducePerKey
  , reducePerKeyParallel
--  , reducePerKeySequential 
  :: (MapReducible mr k2 v2, Binary v2) => 
  Connection -> Int -> mr -> Map k2 Int -> IO (mr)
reducePerKey = reducePerKeyParallel

{-
-- | Reduce Phase. -- The Reduce Phase does not run parallelized.
reducePerKeySequential conn _ initialMR m
 = rpk (uncurry (reduceMR initialMR)) (M.toList m) initialMR 
   where
     rpk :: (MapReducible mr k2 v2, Binary v2) => ((k2, [v2]) -> IO(Maybe mr)) -> [(k2, Int)] -> mr -> IO(mr)
     rpk _  [] result = return result 
     rpk rf l  result
       = do 
         let (k2, v2Id) = head l
         v2s  <- retrieveValues conn v2Id 
         done <- rf (k2,v2s)
         if isJust done
           then rpk rf (drop 1 l) (mergeMR result (fromJust done))
           else rpk rf (drop 1 l) result    
-}



reducePerKeyParallel conn maxWorkers initialMR m = 
  let input   = partitionList maxWorkers (M.toList m)    -- partition input data
      workers = length input                             -- get real worker count
  in 
  do
  chan <- newChan
  connMVar <- newMVar conn
  mapM (runReduceTask connMVar chan initialMR) input
  foldM (\mr _ -> do
                  res <- readChan chan
                  if isJust res then return (mergeMR mr (fromJust res)) else return mr
        ) initialMR [1..workers]
  


runReduceTask :: (MapReducible mr k2 v2, Binary v2) => MVar Connection -> Chan (Maybe mr) ->  mr -> [(k2, Int)] -> IO ()
runReduceTask connMVar chan initialMR input                                  -- start new Thread
  = forkIO                                                                -- spawn new thread
    ( do                                                    
      res <- catch                                                        -- catch exception so the
             ( do                                                         -- MapReduce can continue
               r <- foldM (  \mr (k2, v2Id)                               -- if one worker fails
                           -> do
                              conn <- takeMVar connMVar                   -- exclusive db access
                              v2   <- retrieveValues conn v2Id            -- get v2s from db
                              putMVar connMVar conn                       -- release db access
                              val  <- reduceMR initialMR k2 v2            -- apply rEDUCE function
                              if isJust val                               -- process result
                                 then return (mergeMR mr (fromJust val))
                                 else return mr
                          ) initialMR input
               return $ Just r
             ) 
             (\_ -> return Nothing)  
      writeChan chan res                                                  -- write result to channel  
      return ()
    ) >> return ()

storeValue :: Binary v2 => Connection -> Int -> v2 -> IO ()
storeValue conn di v2 =
  quickQuery conn insertQuery [toSql (di), toSql (B.concat . BL.toChunks $ encode v2)] 
  >> commit conn
  
retrieveValues :: (Binary v2) => Connection -> Int -> IO [v2]
retrieveValues conn di = 
  do
  r <- quickQuery conn lookupQuery [toSql di] 
  mapM (\v -> return $ decode $ BL.fromChunks [fromSql $ head v]) r
  

createConnection :: FilePath -> IO Connection
createConnection f = do
                     conn <- connectSqlite3 f
                     t <- getTables conn
                     if  ("mr" `elem` t)
                       then return conn -- error ("Holumbus.Control.MapReduce.ParallelWithClassPersistent: Table mr already exists in database " ++ f)
                       else quickQuery conn createQuery [] >> commit conn >> return (conn)

createQuery :: String
createQuery = "CREATE TABLE mr (k2id INTEGER, v2 BLOB)"

insertQuery :: String
insertQuery = "INSERT OR REPLACE INTO mr (k2id, v2) VALUES (?, ?)"

lookupQuery :: String
lookupQuery = "SELECT v2 FROM mr WHERE (k2id = ?)"
           