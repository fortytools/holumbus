
module Holumbus.Master.MRMasterServer 
(
    startServer
  , stopServer
) 
where

import Control.Concurrent
import Control.Monad
import Data.Binary
import Network
import System.IO

import Holumbus.Network.Protocol
import qualified Holumbus.Network.Server as Server
import Holumbus.Master.MasterData
import Holumbus.Master.WorkerRegistry
import qualified Holumbus.Master.ProcessScheduler as PS


-- | starts the server of the worker
startServer :: MVar MasterData -> IO (Bool)
startServer dat
  = do
    masterData <- takeMVar dat
    (r, newThreadId) <- case (getServerThreadId masterData) of
      Nothing -> do 
                 putStrLn ("starting server at port " ++ show (getServerPortNumber masterData))
                 i <- forkIO $ Server.listenForRequests (requestDispatcher dat) (getPortId masterData)
                 return (True, Just i)
      Just i  -> do 
                 putStrLn "server already started"
                 return (False, Just i)
    putMVar dat (setServerThreadId newThreadId masterData)
    return r
    where
      getPortId wd = PortNumber (getServerPortNumber wd)

-- | stops the server of the worker
stopServer :: MVar MasterData -> IO (Bool)
stopServer dat
  = do
    masterData <- takeMVar dat
    (r, newServerId) <- case (getServerThreadId masterData) of
      Nothing -> do 
                 putStrLn "no server running"
                 return (False, Nothing)
      Just i  -> do 
                 putStrLn "stopping server"
                 killThread i
                 return (True, Nothing)
    putMVar dat (setServerThreadId newServerId masterData)
    return r

-- | dispatches the server-requests of the worker
requestDispatcher :: MVar MasterData -> Handle -> HostName -> PortNumber -> IO ()
requestDispatcher dat hdl hst _
  = do
   c <- getCommand hdl  
   res <- dispatch (getCommandString c) c
   return res
   where 
     dispatch cmd c | cmd == registerCmd   = handleRegisterWorker dat hst hdl c
                    | cmd == unregisterCmd = handleUnregisterWorker dat hdl c
                    | otherwise            = handleUnknownCommand hdl cmd c
                      
                      
handleRegisterWorker :: MVar MasterData -> HostName -> Handle -> Command -> IO()
handleRegisterWorker dat hst hdl cmd
  = do
    putStrLn "REGISTER-Command"    
    putStrLn ("cmd: " ++ show cmd)
    md <- readMVar dat
    port <- return (getIntegerParameter "PORT" cmd)
    case port of
      (Just p) -> do
                  ps <- takeMVar (getProcessScheduler md)
                  (newId, newPs) <- return (ps' p ps)
                  putMVar (getProcessScheduler md) newPs
                  putCommand (posresp newId) hdl                    
      Nothing  -> do
                  putCommand negresp hdl
                  putStrLn "error - no port found"
    
    --hPutStrLn hdl (successCode ++ " " ++ show newId)
    where 
      newDat port = createWorkerData port hst
      ps' port ps = PS.addWorker (newDat port) ps
      posresp i = addCommandParameter "ID" (encode i) $ mkCommand successCode
      negresp   = addCommandParameter "MSG" (encode "port not found") $ mkCommand failureCode


handleUnregisterWorker :: MVar MasterData -> Handle -> Command -> IO()
handleUnregisterWorker dat hdl cmd 
  = do
    putStrLn "UNREGISTER-Command"
    putStrLn ("cmd: " ++ show cmd)
    md <- readMVar dat
    idx <- return (getIntegerParameter "ID" cmd)
    case idx of
      (Just i) -> do 
                  ps <- takeMVar (getProcessScheduler md)
                  putMVar (getProcessScheduler md) $ PS.deleteWorker i ps                  
                  putCommand posresp hdl
      Nothing  -> do
                  putCommand negresp hdl
                  putStrLn "error - id not found"
    where
      posresp = mkCommand successCode
      negresp = addCommandParameter "MSG" (encode "unknown id") $ mkCommand failureCode


handleUnknownCommand :: Handle -> String -> Command -> IO()
handleUnknownCommand hdl cmd _ 
  = do
    putStrLn ("unknownCommand: " ++ cmd)    
    putCommand negresp hdl
    where
      negresp = addCommandParameter "MSG" (encode "unknown command") $ mkCommand failureCode
