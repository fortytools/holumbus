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
, closeWorker
)
where


import Control.Concurrent
import qualified Control.Exception as E

import Data.Maybe

import System.Log.Logger

import qualified Holumbus.Network.Port as P
import Holumbus.Network.Site
import qualified Holumbus.Distribution.Messages as M
import qualified Holumbus.Distribution.Master.MasterPort as MP
import qualified Holumbus.Distribution.Master as MC
import Holumbus.Distribution.Worker
import Holumbus.Common.Utils

localLogger :: String
localLogger = "Holumbus.Distribution.Master.MasterData"


data WorkerData = WorkerData {
    wd_WorkerId       :: MVar (Maybe M.WorkerId)
  , wd_SiteId         :: ! SiteId
  , wd_ServerThreadId :: MVar (Maybe ThreadId)
  , wd_OwnStream      :: ! M.WorkerRequestStream
  , wd_OwnPort        :: ! M.WorkerRequestPort
  , wd_MasterPort     :: MVar MP.MasterPort
  }
  
  


newWorker :: MP.MasterPort -> IO WorkerData
newWorker mp
  = do
    -- initialize values
    nidMVar <- newMVar Nothing
    sid     <- getSiteId
    tid     <- newMVar Nothing
    st      <- (P.newStream::IO M.WorkerRequestStream)
    po      <- ((P.newPort st)::IO M.WorkerRequestPort)
    mMVar   <- newMVar mp
    let wd'' = (WorkerData nidMVar sid tid st po mMVar)
    -- first, we start the server, because we can't handle requests without it
    wd' <- startRequestDispatcher wd''
    -- then we try to register a the server
    wd  <- registerWorker wd'
    return wd


closeWorker :: WorkerData -> IO ()
closeWorker wd
  = do
    -- shutdown the server thread and the stream
    wd'  <- unregisterWorker wd
    wd'' <- stopRequestDispatcher wd'
    P.closeStream (wd_OwnStream wd'')
    return ()      


startRequestDispatcher :: WorkerData -> IO WorkerData
startRequestDispatcher wd 
  = do
    servId <- takeMVar (wd_ServerThreadId wd)
    servId' <- case servId of
      i@(Just _) -> return i
      (Nothing) ->
        do
        i <- forkIO $ requestDispatcher wd
        return (Just i)
    putMVar (wd_ServerThreadId wd) servId'
    return wd


stopRequestDispatcher :: WorkerData -> IO WorkerData
stopRequestDispatcher wd 
  = do
    servId <- takeMVar (wd_ServerThreadId wd)
    servId' <- case servId of
      (Nothing) -> return Nothing
      (Just i) -> 
        do
        E.throwDynTo i myThreadId
        yield
        return Nothing
    putMVar (wd_ServerThreadId wd) servId'
    return wd

requestDispatcher :: WorkerData -> IO ()
requestDispatcher wd
  = do
    E.handle (\e -> 
      do
      errorM localLogger $ show e
      yield
      requestDispatcher wd
     ) $
      do
      -- read the next message from the stream (block, if no message arrived)
      let stream = (wd_OwnStream wd)
      msg <- P.readStreamMsg stream
      -- extract the data
      let dat = P.getMessageData msg
      debugM localLogger "dispatching new Message... "
      debugM localLogger $ show dat
      -- extract the (possible replyport)
      let replyPort = M.decodeWorkerResponsePort $ P.getGenericData msg
      if (isNothing replyPort)
        then do
          errorM localLogger "no reply port in message"
          yield
        else do
          -- do the dispatching in a new process...
          _ <- forkIO $ dispatch wd dat $ fromJust replyPort
          return ()
      --threadDelay 10
      requestDispatcher wd


dispatch 
  :: WorkerData 
  -> M.WorkerRequestMessage 
  -> M.WorkerResponsePort
  -> IO ()
dispatch wd msg replyPort
  = do
    case msg of
      _ -> 
        handleRequest replyPort (return ()) (\_ -> M.WRspUnknown)


handleRequest
  :: M.WorkerResponsePort
  -> IO a
  -> (a -> M.WorkerResponseMessage) 
  -> IO ()
handleRequest po fhdl fres
  = do
    -- in case, we can't send the error...
    E.handle (\e -> errorM localLogger $ show e) $ do
      do
      -- in case our operation fails, we send a failure-response
      E.handle (\e -> P.send po (M.WRspError $ show e)) $
        do
        -- our action, might raise an exception
        r <- fhdl
        -- send the response
        P.send po $ fres r

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


registerWorker :: WorkerData -> IO (WorkerData)
registerWorker wd
  = do
    debugM localLogger "registering at controller"
    let sid = (wd_SiteId wd)
    let np = (wd_OwnPort wd)
    -- get the new nid
    nid <- withMVar (wd_MasterPort wd) $
      \cp ->
      do  
      (nid, _) <- MC.registerWorker sid np cp
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


instance Worker WorkerData where


  getWorkerRequestPort wd = wd_OwnPort wd
  
  
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
      
      where
        gap = 20