
module Holumbus.Master.HeartBeat 
(
    startHeartBeat
  , stopHeartBeat
)
where

import Control.Exception
import Control.Concurrent
import Network
import System.IO

import Holumbus.Master.MasterData
import Holumbus.Master.WorkerRegistry
import qualified Holumbus.Master.ProcessScheduler as PS
import Holumbus.Network.Client
import qualified Holumbus.Network.Protocol as P

startHeartBeat :: MVar MasterData -> IO (Bool)
startHeartBeat dat
  = do
    md <- takeMVar dat
    (r, newThreadId) <- case (getHeartBeatThreadId md) of
      Nothing -> do 
                 putStrLn "starting heartbeat"
                 i <- forkIO $ beatRegistry dat (fromSecToMicroSec $ getHeartBeatDelay md)
                 return (True, Just i)
      Just i  -> do 
                 putStrLn "heartbeat already started"
                 return (False, Just i)
    putMVar dat (setHeartBeatThreadId newThreadId md)
    return r
    
stopHeartBeat :: MVar MasterData -> IO (Bool)
stopHeartBeat dat
  = do
    md <- takeMVar dat
    (r, newServerId) <- case (getHeartBeatThreadId md) of
      Nothing -> do 
                 putStrLn "no heartbeat running"
                 return (False, Nothing)
      Just i  -> do 
                 putStrLn "stopping heartbeat"
                 killThread i
                 return (True, Nothing)
    putMVar dat (setHeartBeatThreadId newServerId md)
    return r

fromSecToMicroSec :: Int -> Int
fromSecToMicroSec = (*1000000)

beatRegistry :: MVar MasterData -> Int -> IO ()
beatRegistry dat dly
  = do
    putStrLn "pinging workers"
    md <- readMVar dat
    putStrLn "1"
    psMVar <- return (getProcessScheduler md)
    putStrLn "2"    
    tempPs <- readMVar psMVar
    putStrLn "3"
    res <- sequence $ beatWorkers (PS.getWorkers tempPs)
    putStrLn "4"
    oldPs <- takeMVar psMVar
    putStrLn "5"
    putMVar psMVar (PS.updateWorkerLifeTime res oldPs)
    putStrLn "waiting..."
    threadDelay dly
    putStrLn "finished... again"
    beatRegistry dat dly 
    
    
beatWorkers :: [WorkerData] -> [IO (WorkerId, Bool)]
beatWorkers w = map send w
    where
      send :: WorkerData -> IO (WorkerId, Bool)
      send wd 
        = do
          putStrLn "b1"
          -- TODO exception handling one level deeper
          handle (\e -> do
                        putStrLn (show e)
                        return (i, False)) $
            do
            putStrLn "b2"
            res <- sendRequest (sendBeat) hn (PortNumber $ fromInteger po)
            putStrLn "b3"
            return (i, res)
         where
           i  = getWorkerId wd
           hn = getHostName wd
           po = getPortNumber wd

sendBeat :: Handle -> IO (Bool)
sendBeat hdl
  = do
    P.putCommand (P.mkCommand P.pingCmd) hdl
    rsp <- P.getCommand hdl    
    if ((P.getCommandString rsp) == P.successCode) 
      then
        do
        return True
      else
        do
        return False