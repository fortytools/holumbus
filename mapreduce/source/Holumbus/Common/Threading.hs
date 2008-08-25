-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Common.Threading
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Operations to start and stop threads which will not be killed when a regular
  exception occurs. In this case, the thread will continue working. Such a 
  thread can only be killed by the stop-method.
  
-}

-- ----------------------------------------------------------------------------

module Holumbus.Common.Threading
(
  startThread
, stopThread
)
where

import           Control.Concurrent
import qualified Control.Exception as E
import           Data.Maybe
import           System.Log.Logger


localLogger :: String
localLogger = "Holumbus.Common.Threading"

-- ----------------------------------------------------------------------------
-- Thread-Control
-- ----------------------------------------------------------------------------

startThread
  :: MVar (Maybe ThreadId)     -- ^ threadId of the watcher, to be filled
  -> Int                       -- ^ delay value in ns
  -> IO ()                     -- ^ action to be done if ping does not success 
  -> IO ()
startThread mVarTid d f
  = modifyMVar mVarTid $
      \servId ->
      do
      servId' <- case servId of
        i@(Just _) -> return i
        (Nothing) ->
          do
          i <- forkIO $ doAction d f
          return (Just i)
      return (servId',())
    where
    doAction delay fct
      = E.handle (\e -> 
          do
          -- if a normal exception occurs, the thread should not be killed
          warningM localLogger $ show e
          yield
          doAction delay fct
         ) $
          do
          -- catch the exception which tells us to kill the dispatcher and kill it
          E.catchDyn (doAction') (handler)
        where
        handler :: ThreadId -> IO ()
        handler i 
          = do
            debugM localLogger $ "thread normally closed by other thread " ++ show i
        doAction'
          = do
            -- wait for next watch-cycle
            threadDelay delay
            -- do the action
            fct
            -- and again
            doAction delay fct


stopThread
  :: MVar (Maybe ThreadId)     -- ^ threadId of the watcher, to be filled
  -> IO ()
stopThread mVarTid
  = modifyMVar mVarTid $
      \servId ->
      do
      servId' <- case servId of
        (Nothing) -> return Nothing
        (Just i) -> 
          do
          me <- myThreadId
          E.throwDynTo i me
          yield
          return Nothing
      return (servId',())