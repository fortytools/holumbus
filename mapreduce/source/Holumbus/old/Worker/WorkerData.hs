
module Holumbus.Worker.WorkerData where

import Control.Concurrent
import Network

import Holumbus.Task.TaskProcessor

data WorkerData = MkWorkerData { 
                    serverThreadId   :: Maybe ThreadId -- | own server thread 
                  , serverPortNumber :: Integer        -- | own portnumber
                  , ownId            :: Maybe Integer  -- | own workerId
                  , masterHostName   :: HostName       -- | master hostname
                  , masterPortNumber :: Integer        -- | master portnumber
                  , taskLookUpMap    :: TaskLookUpMap  -- | the tasks, which can be processed by the worker
                  } deriving (Show, Eq)

initializeWorkerData :: TaskLookUpMap -> WorkerData
initializeWorkerData taskmap
  = MkWorkerData
    Nothing
    4243
    Nothing
    "localhost"
    4242
    taskmap

setServerThreadId :: Maybe ThreadId -> WorkerData -> WorkerData
setServerThreadId i d = d { serverThreadId = i }

setOwnId :: Maybe Integer -> WorkerData -> WorkerData
setOwnId i d = d { ownId = i }

getServerThreadId :: WorkerData -> Maybe ThreadId
getServerThreadId d = serverThreadId d

getServerPortNumber :: WorkerData -> PortNumber
getServerPortNumber d = fromInteger $ serverPortNumber d

getOwnId :: WorkerData -> Maybe Integer
getOwnId d = ownId d

getMasterHostName :: WorkerData -> HostName
getMasterHostName d = masterHostName d

getMasterPortNumber :: WorkerData -> PortNumber
getMasterPortNumber d = fromInteger $ masterPortNumber d

getTaskLookUpMap :: WorkerData -> TaskLookUpMap
getTaskLookUpMap d = taskLookUpMap d

printWorkerData :: WorkerData -> IO ()
printWorkerData d
  = do
    putStrLn ("ServerThreadId: " ++ (show $ serverThreadId d))
    putStrLn ("OwnPort:        " ++ (show $ serverPortNumber d))
    putStrLn ("own Id:         " ++ (show $ ownId d))
    putStrLn ("Master Host:    " ++ (show $ masterHostName d))
    putStrLn ("Master Port:    " ++ (show $ masterPortNumber d))
    putStrLn ("Tasks:          " ++ (show $ taskLookUpMap d))