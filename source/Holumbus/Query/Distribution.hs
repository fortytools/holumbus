-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Distribution
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Process a Holumbus query in distributed (concurrent) manner.

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-type-defaults  #-}

module Holumbus.Query.Distribution 
  (
  -- * Distribution types
  DistributedConfig (..)
  , Server
  
  -- * Processing
  , processDistributed
  )
where

import System.IO

import Control.Concurrent
import Control.Exception
import Control.Monad

import Network

import Data.Binary

import Codec.Compression.BZip

import qualified Data.ByteString.Lazy as B

import Holumbus.Index.Common
import Holumbus.Query.Intermediate hiding (null)
import Holumbus.Query.Result (Result)
import Holumbus.Query.Language

-- | The configuration for distributed query processing.
data DistributedConfig = DistributedConfig
  { queryServers   :: ![Server] -- ^ The list of query server to use for processing a query.
  , compressResult :: !Bool     -- ^ Indicates whether compression should be used when transmitting a result.
  }

-- | The identification of a query server. Should take the form @hostname:port@ or just 
-- @hostname@ if the default port (4242) should be used.
type Server = String

-- | A global registry for worker threads.
type Workers = MVar [Worker]
-- | A single worker thread.
type Worker = MVar ()

-- | The default port for query servers.
defaultPort :: PortNumber
defaultPort = 4242

-- | Process a query in distributed manner.
processDistributed :: (HolDocuments d) => DistributedConfig -> d -> Query -> IO Result
processDistributed cfg d q = 
  do
  result <- newMVar emptyIntermediate

  workers <- startWorkers (sendQuery result (compressResult cfg) q) (queryServers cfg)
  waitForWorkers workers

  combined <- takeMVar result
  return (toResult d combined)

-- | Send the query to a server and merge the result with the global result.
sendQuery :: MVar Intermediate -> Bool -> Query -> Server -> IO ()
sendQuery i c q s = 
  withSocketsDo $ do bracket (connectTo (getHost s) (getPort s)) (hClose) (sendQuery')
    where
    sendQuery' hdl = 
      do
      hSetBuffering hdl NoBuffering
      enc <- return (encode q)
      -- Tell the server the length of the ByteString to expect.
      hPutStrLn hdl (show $ B.length enc)
      B.hPut hdl enc
      -- Get the length of the ByteString to expect.
      len <- liftM read $ hGetLine hdl   
      raw <- B.hGet hdl len
      -- Decode (and possibly decompress) the result
      result <- if c then return (decode . decompress $ raw) else return (decode raw)
      -- Merge with the global result.
      current <- takeMVar i
      combined <- return (union current result)
      putMVar i combined

-- | Extract a host name from the server string.
getHost :: Server -> HostName
getHost s = takeWhile (/= ':') s

-- | Try to extract a port identifier from the server string. Fault tolerance needs to be improved.
getPort :: Server -> PortID
getPort s = let r = dropWhile (/= ':') s in
              if null r then PortNumber defaultPort else
                if null $ tail r then PortNumber defaultPort else
                  PortNumber (fromIntegral $ read $ tail r)

-- | Start an IO action for every value of a list and return the spawned workers.
startWorkers :: (a -> IO ()) -> [a] -> IO Workers
startWorkers w d = 
  do
  workers <- newMVar []            -- Initialize the list of all workers.
  foldM (startWorker w) workers d  -- Add a worker for every input value.

-- | Start a single IO action for a value and register it with the other workers.
startWorker :: (a -> IO ()) -> Workers -> a -> IO Workers
startWorker w ws d = 
  do
  worker <- newEmptyMVar                     -- Create a MVar to represent this worker.
  workers <- takeMVar ws                     -- Get the list of all workers.
  putMVar ws (worker:workers)                -- Add this worker to the list.
  forkIO ((w d) `finally` putMVar worker ()) -- Start the worker and make sure to signal success on termination.
  return ws

-- | Wait for the workers started by @startWorkers@.
waitForWorkers :: Workers -> IO ()
waitForWorkers ws = 
  do
  workers <- takeMVar ws       -- Get the list of all workers.
  case workers of
    []   -> return ()          -- All workers have finished.
    m:ms -> do
            putMVar ws ms      -- Make the list of remaining workers avaliable again.
            takeMVar m         -- Wait for the first worker from the list.
            waitForWorkers ws  -- Wait for all remaining workers.

