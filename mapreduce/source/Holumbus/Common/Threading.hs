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

{-# OPTIONS -fglasgow-exts #-}
module Holumbus.Common.Threading
(
  Thread
, newThread
, setThreadDelay
, setThreadAction
, setThreadErrorHandler

, startThread
, stopThread
)
where

import           Control.Concurrent
import qualified Control.Exception as E
import           Data.Maybe
import           Data.Typeable
import           System.Log.Logger


localLogger :: String
localLogger = "Holumbus.Common.Threading"


-- ----------------------------------------------------------------------------
-- Thread-Control
-- ----------------------------------------------------------------------------

data KillThreadException = KillThreadException ThreadId
  deriving (Typeable)


data ThreadData = ThreadData {
    thd_Id      :: Maybe ThreadId
  , thd_Running :: Bool
  , thd_Delay   :: Maybe Int
  , thd_Action  :: (IO ())
  , thd_Error   :: (IO ())
  }
  
type Thread = MVar ThreadData 


newThread :: IO Thread
newThread
  = do 
    newMVar $ ThreadData Nothing False defaultDelay noAction noAction
    where
    noAction     = return ()
    defaultDelay = Nothing


setThreadDelay :: Int -> Thread -> IO ()
setThreadDelay d thread
  = do
    modifyMVar thread $ \thd -> return (thd {thd_Delay = Just d},())
    
    
setThreadAction :: (IO ()) -> Thread -> IO ()
setThreadAction f thread
  = do
    modifyMVar thread $ \thd -> return (thd {thd_Action = f},())


setThreadErrorHandler :: (IO ()) -> Thread -> IO ()
setThreadErrorHandler e thread
  = do
    modifyMVar thread $ \thd -> return (thd {thd_Error = e},())


startThread :: Thread -> IO ()
startThread th
  = modifyMVar th $
      \thd ->
      do
      servId' <- case (thd_Id thd) of
        i@(Just _) -> return i
        (Nothing) ->
          do
          i <- forkIO $ doAction th
          return (Just i)
      return (thd {thd_Id = servId', thd_Running = True},())
    where    
    doAction thread
      = do
        thd <- readMVar thread
        if (thd_Running thd)
          -- if thread is still running 
          then do
            E.handle 
             (\e -> do
              -- if a normal exception occurs, the error handler should be excuted
              warningM localLogger $ show e
              (thd_Error thd)
              doAction thread
             ) $
              do
              -- catch the exception which tells us to kill the dispatcher and kill it
              E.catchDyn
               (do
                -- wait for next watch-cycle
                if (isJust (thd_Delay thd)) 
                  then do threadDelay $ fromJust (thd_Delay thd)
                  else do return ()
                -- do the action
                (thd_Action thd)
                -- and again
                doAction thread
               )
               (killHandler) 
          else do
            debugM localLogger $ "thread normally closed by himself"
            deleteThreadId thread
        where
        killHandler :: KillThreadException -> IO ()
        killHandler (KillThreadException i)
          = do
            debugM localLogger $ "thread normally closed by other thread " ++ show i
            deleteThreadId thread
        deleteThreadId :: Thread -> IO ()
        deleteThreadId t
          = modifyMVar t $ \thd -> return (thd {thd_Id = Nothing, thd_Running = False},())


stopThread :: Thread -> IO ()
stopThread thread
  = do
    me <- myThreadId
    him <- withMVar thread $ \thd -> return $ thd_Id thd
    if (isJust him) 
      then do
        let he = fromJust him
        if (me == he) 
          -- if stop is called from the thread, we want to finish our work
          then do 
            modifyMVar thread $ \thd -> return (thd {thd_Running = False},())
          -- else we kill it the unfriendly way...
          else do
            E.throwDynTo he (KillThreadException me)
      else do
        return ()