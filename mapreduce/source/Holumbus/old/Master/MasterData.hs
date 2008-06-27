
module Holumbus.Master.MasterData
(
  MasterData
, initializeMasterData
, setHeartBeatThreadId
, setServerThreadId
, setServerPortNumber
, getHeartBeatThreadId
, getHeartBeatDelay
, getServerThreadId 
, getServerPortNumber
--, getWorkerRegistry
, getProcessScheduler
, printMasterData
)
where

import Control.Concurrent
import Network

import Holumbus.Master.ProcessScheduler
--import Holumbus.Master.WorkerRegistry 
import Holumbus.Task.TaskProcessor

data MasterData = MkMasterData {
                    heartBeatThreadId :: Maybe ThreadId      -- ^ ThreadId of the heartBeat process
                  , heartBeatDelay    :: Int                 -- ^ Delay of the heartBeat in sec
                  , serverThreadId    :: Maybe ThreadId      -- ^ ThreadId of the server thread 
                  , serverPortNumber  :: Integer             -- ^ PortNumber of the server
--                  , workerRegistry    :: MVar WorkerRegistry -- ^ Workerregistry
                  , processScheduler  :: MVar ProcessScheduler -- ^ProcessScheduler
                  , taskNameSet       :: TaskNameSet         -- ^ the names of the available Tasks
                  }

initializeMasterData :: TaskNameSet -> IO (MasterData)
initializeMasterData ts
  = do
    --reg <- newMVar emptyWorkerRegistry 
    prsc <- newMVar emptyProcessScheduler
    md  <- return (MkMasterData Nothing 10 Nothing 4242 prsc ts)
    return md

setHeartBeatThreadId :: Maybe ThreadId -> MasterData -> MasterData
setHeartBeatThreadId i d  = d { heartBeatThreadId = i }

setServerThreadId :: Maybe ThreadId -> MasterData -> MasterData
setServerThreadId i d = d { serverThreadId = i }

setServerPortNumber :: Integer -> MasterData -> MasterData
setServerPortNumber i d = d { serverPortNumber = i }

getHeartBeatThreadId :: MasterData -> Maybe ThreadId
getHeartBeatThreadId d = heartBeatThreadId d

getHeartBeatDelay :: MasterData -> Int
getHeartBeatDelay d = heartBeatDelay d

getServerThreadId :: MasterData -> Maybe ThreadId
getServerThreadId d = serverThreadId d

getServerPortNumber :: MasterData -> PortNumber
getServerPortNumber d = fromInteger $ serverPortNumber d

--getWorkerRegistry :: MasterData -> MVar WorkerRegistry
--getWorkerRegistry d = workerRegistry d

getProcessScheduler :: MasterData -> MVar ProcessScheduler
getProcessScheduler d = processScheduler d

printMasterData :: MasterData -> IO ()
printMasterData d
  = do
    putStrLn ("HeartBeatThreadId: " ++ (show $ heartBeatThreadId d))
    putStrLn ("ServerThreadId:    " ++ (show $ serverThreadId d))
    putStrLn ("OwnPort:           " ++ (show $ serverPortNumber d))
    putStrLn ("Tasks:             " ++ (show $ taskNameSet d))
