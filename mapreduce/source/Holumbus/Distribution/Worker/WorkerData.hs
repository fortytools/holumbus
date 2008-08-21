-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Distribution.Worker.WorkerData
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.Distribution.Worker.WorkerData
(
-- * Datatypes
  WorkerData
  
-- * creation and destruction
, newWorker
)
where


import           Control.Concurrent
import qualified Control.Exception as E

import           Data.Maybe
-- import qualified Data.Set as Set

import           System.Log.Logger

import qualified Holumbus.FileSystem.FileSystem as FS
import           Holumbus.Network.Site
import           Holumbus.Network.Port
import           Holumbus.Network.Messages
import           Holumbus.MapReduce.Types
import qualified Holumbus.MapReduce.TaskProcessor as TP
import qualified Holumbus.Distribution.Messages as M
import qualified Holumbus.Distribution.Master.MasterPort as MP
import qualified Holumbus.Distribution.Master as MC
import           Holumbus.Distribution.Worker
import           Holumbus.Common.Utils

localLogger :: String
localLogger = "Holumbus.Distribution.Worker.WorkerData"



data WorkerData = WorkerData {
    wd_WorkerId       :: MVar (Maybe M.WorkerId)
  , wd_SiteId         :: ! SiteId
  , wd_ServerThreadId :: MVar (Maybe ThreadId)
  , wd_OwnStream      :: ! M.WorkerRequestStream
  , wd_OwnPort        :: ! M.WorkerRequestPort
  , wd_MasterPort     :: MVar MP.MasterPort
  , wd_TaskProcessor  :: TP.TaskProcessor   
  }
  
  


newWorker :: FS.FileSystem -> ActionMap -> MP.MasterPort -> IO WorkerData
newWorker fs am mp
  = do
    -- initialize values
    nidMVar <- newMVar Nothing
    sid     <- getSiteId
    tid     <- newMVar Nothing
    st      <- (newLocalStream Nothing::IO M.WorkerRequestStream)
    po      <- newPortFromStream st
    mpMVar  <- newMVar mp
    tp      <- TP.newTaskProcessor
    
    -- configure the TaskProcessor
    TP.setFileSystemToTaskProcessor fs tp
    TP.setActionMap am tp
    TP.setTaskCompletedHook (sendTaskCompleted mpMVar) tp
    TP.setTaskErrorHook (sendTaskError mpMVar) tp
    TP.startTaskProcessor tp
    
    let wd' = (WorkerData nidMVar sid tid st po mpMVar tp)
    -- first, we start the server, because we can't handle requests without it
    startRequestDispatcher (wd_ServerThreadId wd') st (dispatch wd')
    -- then we try to register a the server
    wd  <- registerWorker wd'
    return wd


dispatch 
  :: WorkerData 
  -> M.WorkerRequestMessage 
  -> M.WorkerResponsePort
  -> IO ()
dispatch wd msg replyPort
  = do
    case msg of
      (M.WReqStartTask td) ->
        do
        handleRequest replyPort (startTask td wd) (\_ -> M.WRspSuccess)
        return ()
      (M.WReqStopTask tid) ->
        do
        handleRequest replyPort (stopTask tid wd) (\_ -> M.WRspSuccess)
        return ()
      (M.WReqStopAllTasks) ->
        do
        handleRequest replyPort (stopAllTasks wd) (\_ -> M.WRspSuccess)
        return ()
      _ -> 
        handleRequest replyPort (return ()) (\_ -> M.WRspUnknown)


-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


registerWorker :: WorkerData -> IO (WorkerData)
registerWorker wd
  = do
    debugM localLogger "registering at controller"
    let sid = (wd_SiteId wd)
    let np = (wd_OwnPort wd)
    as <- TP.getActionNames (wd_TaskProcessor wd)
    -- get the new nid
    nid <- withMVar (wd_MasterPort wd) $
      \mp ->
      do  
      (nid, _) <- MC.registerWorker sid np as mp
      return (Just nid)
    -- write the new nodeId in the record
    modifyMVar (wd_WorkerId wd) (\_ -> return (nid,wd)) 
        

unregisterWorker :: WorkerData -> IO (WorkerData)
unregisterWorker wd
  = do
    debugM localLogger "unregistering at controller"
    nid <- readMVar (wd_WorkerId wd)
    unregister nid
    modifyMVar (wd_WorkerId wd) (\_ -> return (Nothing,wd))
    where
      unregister Nothing = return ()
      unregister (Just i)
        = do        
          withMVar (wd_MasterPort wd) $
            \cp -> MC.unregisterWorker i cp
          return ()



-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


sendTaskCompleted :: MVar MP.MasterPort -> TaskData -> IO Bool
sendTaskCompleted mvmp td
  = E.handle (\_ -> return False) $
      modifyMVar mvmp $
        \mp ->
        do
        debugM localLogger $ "completed Task" ++ show (td_TaskId td)
        MC.receiveTaskCompleted td mp
        return (mp, True)


sendTaskError :: MVar MP.MasterPort -> TaskData -> IO Bool
sendTaskError mvmp td
  = E.handle (\_ -> return False) $
      modifyMVar mvmp $
        \mp ->
        do
        debugM localLogger $ "error Task" ++ show (td_TaskId td)
        MC.receiveTaskError td mp
        return (mp, True)


-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------



instance Worker WorkerData where

  closeWorker wd
    = do
      -- shutdown the server thread and the stream
      wd'  <- unregisterWorker wd
      stopRequestDispatcher (wd_ServerThreadId wd')
      closeStream (wd_OwnStream wd')
      return ()

  getWorkerRequestPort wd = wd_OwnPort wd
  
  
  startTask td wd
    = do
      let tp = (wd_TaskProcessor wd)
      debugM localLogger $ "executing Task" ++ show (td_TaskId td)
      TP.startTask td tp
      return wd
  
  
  stopTask tid wd
    = do
      let tp = (wd_TaskProcessor wd)
      debugM localLogger $ "stopping Task" ++ show tid      
      TP.stopTask tid tp
      return wd
    

  stopAllTasks wd
    = do
      let tp = (wd_TaskProcessor wd)
      debugM localLogger "stopping all Tasks"
      TP.stopAllTasks tp
      return wd

  
  printDebug wd
    = do
      putStrLn "Worker-Object (full)"
      withMVar (wd_WorkerId wd) $
        \wid -> putStrLn $ prettyRecordLine gap "WorkerId:" wid                      
      withMVar (wd_ServerThreadId wd) $ 
        \i-> do putStrLn $ prettyRecordLine 15 "ServerId:" i
      putStrLn $ prettyRecordLine gap "SiteId:" (wd_SiteId wd)
      putStrLn $ prettyRecordLine gap "OwnStream:" (wd_OwnStream wd)
      putStrLn $ prettyRecordLine gap "OwnPort:" (wd_OwnPort wd)
      withMVar (wd_MasterPort wd) $
        \mp -> do putStrLn $ prettyRecordLine gap "MasterPort:" mp
      tp <- TP.printTaskProcessor (wd_TaskProcessor wd)
      putStrLn "TaskProcessor:"
      putStrLn tp
      where
        gap = 20