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
  , Hook
  
  -- * Client
  , processDistributed
  
  -- * Server
  , listenForRequests
  )
where

import System.IO
import System.CPUTime

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
import Holumbus.Query.Processor
import Holumbus.Query.Fuzzy

-- | The configuration for distributed query processing.
data DistributedConfig = DistributedConfig
  { queryServers   :: ![Server]      -- ^ The list of query server to use for processing a query.
  , compressResult :: !Bool          -- ^ Indicates whether compression should be used when transmitting a result.
  , processConfig  :: !ProcessConfig -- ^ The configuration for processing a query.
  }

type Hook = HostName -> PortNumber -> Query -> Integer -> Integer -> Intermediate -> String -> IO ()

-- | The configuration that will be sent to the server. It contains the query itself, a flag
-- indicating whether compression is requested or not and the configuration for the processor.
type Request = (Query, Bool, FuzzyConfig)

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

-- | This function represents the client side. Processes a query in distributed manner. 
-- The query will be optimized before sending to the query servers. The queries will be sent
-- in parallel, by spawning a dedicated thread for each server.
processDistributed :: (HolDocuments d) => DistributedConfig -> d -> Query -> IO Result
processDistributed cfg d q = 
  do
  result <- newMVar emptyIntermediate

  workers <- startWorkers (sendQuery result request) (queryServers cfg)
  waitForWorkers workers

  combined <- takeMVar result
  return (toResult d combined)
    where
    request = (oq, (compressResult cfg), fuzzyConfig $ processConfig cfg)
    oq = if optimizeQuery $ processConfig cfg then optimize q else q

-- | Send the query to a server and merge the result with the global result.
sendQuery :: MVar Intermediate -> Request -> Server -> IO ()
sendQuery i r@(_, c, _) s = 
  withSocketsDo $ do bracket (connectTo (getHost s) (getPort s)) (hClose) (sendQuery')
    where
    sendQuery' hdl = 
      do
      hSetBuffering hdl NoBuffering
      enc <- return (encode r)
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
      combined <- return $! (union current result)
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

-- | This function represents the server side. It opens a socket on the provided port and 
-- starts listening for requests. A post-processing hook is provided to allow some actions on
-- the processing results, e.g. some logging functionality.
listenForRequests :: HolIndex i => i -> PortNumber -> Hook -> IO ()
listenForRequests i p h =
  do
  idx <- newMVar i
  socket <- listenOn (PortNumber p)
  waitForRequests idx socket h

waitForRequests :: HolIndex i => MVar i -> Socket -> Hook -> IO ()
waitForRequests idx socket h = 
  do
  client <- accept socket
  forkIO $ answerRequest idx client h  -- Spawn new thread to answer the current request.
  waitForRequests idx socket h         -- Wait for more requests.

answerRequest :: HolIndex i => MVar i -> (Handle, HostName, PortNumber) -> Hook -> IO ()
answerRequest i client h = 
  bracket (return client) (\(hdl, _, _) -> hClose hdl) (\cl -> answerRequest' cl)
    where
    answerRequest' (hdl, host, port) = 
      do
      hSetBuffering hdl NoBuffering
      idx <- readMVar i

      start <- getCPUTime
      -- Read the length of what to expect.
      len <- liftM read $ hGetLine hdl
      -- Red and decode the request.
      raw <- B.hGet hdl len
      (query, c, fuzzyCfg) <- return (decode raw)
      -- Process the query
      result <- return (processPartial (ProcessConfig fuzzyCfg False) idx query)
      -- Encode and compress (if requested) the result.
      enc <- if c then return (compress . encode $ result) else return (encode result)
      -- Tell the client the size of the result to expect.
      size <- return (show $ B.length enc)
      hPutStrLn hdl size
      -- Push the result over to the client.
      B.hPut hdl enc
      end <- getCPUTime
      -- Call the hook provided by the user.
      h host port query start end result size
