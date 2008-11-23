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
  thread can only be killed by the stop-method. This whole thing is a wrapper
  around the normal lightweight thread functions.
  
  The created threads execute a function in an infinite loop. This is the
  normal usecase for message dispatcher threads.
  
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

-- | Exception to stop a thread, so we can distinguish this request from
--   "real" exceptions.
data KillThreadException = KillThreadException ThreadId
  deriving (Typeable)


-- | The data needed to access and control the thread. 
data ThreadData = ThreadData {
    thd_Id      :: Maybe ThreadId
  , thd_Running :: Bool
  , thd_Delay   :: Maybe Int
  , thd_Action  :: (IO ())
  , thd_Error   :: (IO ())
  }
  
  
-- | The thread datatype
type Thread = MVar ThreadData 


-- | Creates a new thread object. The thread will not be running.
newThread :: IO Thread
newThread
  = do 
    newMVar $ ThreadData Nothing False defaultDelay noAction noAction
    where
    noAction     = return ()
    defaultDelay = Nothing


-- | Sets the delay between two loop cycles. Default value: no delay.
setThreadDelay :: Int -> Thread -> IO ()
setThreadDelay d thread
  = do
    modifyMVar thread $ \thd -> return (thd {thd_Delay = Just d},())
    
    
-- | Sets the action function, which will be executed in each cycle
setThreadAction :: (IO ()) -> Thread -> IO ()
setThreadAction f thread
  = do
    modifyMVar thread $ \thd -> return (thd {thd_Action = f},())


-- | Sets the error handler. It is activated, when the action function
--   will raise an exception.
setThreadErrorHandler :: (IO ()) -> Thread -> IO ()
setThreadErrorHandler e thread
  = do
    modifyMVar thread $ \thd -> return (thd {thd_Error = e},())


-- | Starts the thread.
startThread :: Thread -> IO ()
startThread th
  = modifyMVar th $
      \thd ->
      do
      -- check, if thread is running (it has a threadId)
      servId' <- case (thd_Id thd) of
        i@(Just _) -> return i
        (Nothing) ->
          do
          -- if not running, start the loop
          i <- forkIO $ doAction th
          return (Just i)
      return (thd {thd_Id = servId', thd_Running = True},())
    where    
    -- the action loop
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


-- | Stops the thread. If the thread itself wants to stop from within the action
--   function, the current cycle will be executed till the end. So statements
--   after this function will still be executed.
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