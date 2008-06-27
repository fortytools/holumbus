
module Holumbus.Master.MasterClient
(
  startTask
)
where

import Control.Concurrent
import Control.Monad
import Data.Binary
import Network
import System.IO

import Holumbus.Network.Client
import Holumbus.Network.Protocol
import Holumbus.Master.WorkerRegistry
import Holumbus.Task.TaskData


sendCommandToWorker :: WorkerData  -> (Handle -> IO (Bool)) -> IO (Bool)
sendCommandToWorker (_, po, hn, _) f
  = do
    sendRequest f hn (PortNumber $ fromInteger po)


startTask :: MVar WorkerRegistry -> WorkerId -> TaskData -> IO (Bool)
startTask dat i t
  = do
    reg <- readMVar dat
    success <- case (getWorker i reg) of
      (Just w) ->
        do
        putStrLn ("start Task at Worker " ++ show w) 
        sendCommandToWorker w (sendStartCommand t)
      _ ->
        do
        putStrLn "worker not found"
        return False
    return success
    
sendStartCommand :: TaskData -> Handle -> IO (Bool)
sendStartCommand t hdl
  = do
    putCommand startReq hdl
    rsp <- getCommand hdl
    if ((getCommandString rsp) == successCode) 
      then
        do
        putStrLn "success"
        return True
      else
        do
        putStrLn "failure"
        return False
    where
      startReq = addCommandParameter "TASK" (encode t) $ mkCommand startTaskCmd