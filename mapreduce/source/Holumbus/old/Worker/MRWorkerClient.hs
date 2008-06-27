
module Holumbus.Worker.MRWorkerClient 
(
    registerWorker
  , unregisterWorker
)
where

import Control.Concurrent
import Data.Binary
--import qualified Data.ByteString.Lazy as B
import Network
import System.IO

import Holumbus.Network.Client
import Holumbus.Network.Protocol
import Holumbus.Worker.WorkerData

registerWorker :: MVar WorkerData -> IO ()
registerWorker dat
  = do
    wd <- readMVar dat
    case (getOwnId wd) of
      Nothing -> do
                 sendCommandToMaster dat (sendRegisterCommand dat)
      _       -> do
                 putStrLn "already registered"

unregisterWorker :: MVar WorkerData -> IO ()
unregisterWorker dat
  = do
    wd <- readMVar dat
    case (getOwnId wd) of
      Nothing -> do
                 putStrLn "not registered"
      _       -> do
                 sendCommandToMaster dat (sendUnregisterCommand dat)

sendCommandToMaster :: MVar WorkerData -> (Handle -> IO ()) -> IO ()
sendCommandToMaster dat f
  = do
    wd <- readMVar dat
    sendRequest f (getMasterHostName wd) (PortNumber $ getMasterPortNumber wd)

sendRegisterCommand :: MVar WorkerData -> Handle -> IO ()
sendRegisterCommand dat hdl
  = do
    wd <- readMVar dat
    putStrLn ("cmd: " ++ show (regCmd $ toInteger $ getServerPortNumber wd))
    putCommand (regCmd $ toInteger $ getServerPortNumber wd) hdl
    rsp <- getCommand hdl
    putStrLn $ show rsp
    if ((getCommandString rsp) == successCode) 
      then
        do
        putStrLn "success"
        i <- return (getIntegerParameter "ID" rsp)
        case i of
          (Just _) -> do
                      d <- takeMVar dat
                      putMVar dat (setOwnId i d)                      
          Nothing  -> do                      
                      putStrLn "error - no id found"
      else
        do
        putStrLn "failure"
    
    where
      regCmd p = addCommandParameter "PORT" (encode p) $ mkCommand registerCmd
      
    
sendUnregisterCommand :: MVar WorkerData -> Handle -> IO ()
sendUnregisterCommand dat hdl
  = do
    wd <- readMVar dat
    putStrLn ("cmd: " ++ show (unregCmd $ strip $ getOwnId wd))
    putCommand (unregCmd $ strip $ getOwnId wd) hdl
    rsp <- getCommand hdl
    if ((getCommandString rsp) == successCode) 
      then
        do
        d <- takeMVar dat
        putMVar dat (setOwnId Nothing d)       
      else
        do
        putStrLn "failure"
    where
      unregCmd i = addCommandParameter "ID" (encode i) $ mkCommand unregisterCmd
      strip (Just i) = i
      strip Nothing  = undefined -- catched in the previous function