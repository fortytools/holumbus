-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Control.Registry
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  This module provides a simple registry facility for worker threads.
  Every worker thread registeres itself with the registry and reregisteres
  when finished. This allowes a controlling thread to be able to wait for
  a number of worker threads to finish.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Control.Registry 
  (
  -- * Registry type
  Registry
  
  -- * Starting workers
  , startWorkers
  
  -- * Waiting for workers
  , waitForWorkers
  )
where

import Control.Exception
import Control.Concurrent
import Control.Monad

-- | A global registry for worker threads.
type Registry = MVar [Worker]
-- | A single worker thread.
type Worker = MVar ()

-- | Start a worker for every value of a list of input data. The workers will register
-- themselves with the registry, which is returned after spawning the threads.
startWorkers :: (a -> IO ()) -> [a] -> IO Registry
startWorkers w d = 
  do
  workers <- newMVar []            -- Initialize the list of all workers.
  foldM (startWorker w) workers d  -- Add a worker for every input value.

-- | Start a single IO action for a value and register it with the other workers.
startWorker :: (a -> IO ()) -> Registry -> a -> IO Registry
startWorker w ws d = 
  do
  worker <- newEmptyMVar                     -- Create a MVar to represent this worker.
  workers <- takeMVar ws                     -- Get the list of all workers.
  putMVar ws (worker:workers)                -- Add this worker to the list.
  forkIO ((w d) `finally` putMVar worker ()) -- Start the worker and make sure to signal success on termination.
  return ws

-- | Wait for the workers registered with the registry to finish.
waitForWorkers :: Registry -> IO ()
waitForWorkers ws = 
  do
  workers <- takeMVar ws       -- Get the list of all workers.
  case workers of
    []   -> return ()          -- All workers have finished.
    m:ms -> do
            putMVar ws ms      -- Make the list of remaining workers avaliable again.
            takeMVar m         -- Wait for the first worker from the list.
            waitForWorkers ws  -- Wait for all remaining workers.
