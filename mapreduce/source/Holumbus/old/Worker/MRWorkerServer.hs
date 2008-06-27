
module Holumbus.Worker.MRWorkerServer
(
    startServer
  , stopServer
) 
where

import Control.Concurrent
import Control.Monad
import Data.Binary
--import qualified Data.List as List
import Network
import System.IO

import qualified Holumbus.Network.Server as Server
import Holumbus.Network.Protocol
import Holumbus.Task.TaskProcessor
import Holumbus.Worker.WorkerData

-- for the MR - testcase
--import Holumbus.Control.MapReduce.Parallel

-- | starts the server of the worker
startServer :: MVar WorkerData -> IO (Bool)
startServer dat
  = do
    workerData <- takeMVar dat
    (r, newThreadId) <- case (getServerThreadId workerData) of
      Nothing -> do 
                 putStrLn ("starting server at port " ++ show (getServerPortNumber workerData))
                 i <- forkIO $ Server.listenForRequests (requestDispatcher dat) (getPortId workerData)
                 return (True, Just i)
      Just i  -> do 
                 putStrLn "server already started"
                 return (False, Just i)
    putMVar dat (setServerThreadId newThreadId workerData)
    return r
    where
      getPortId wd = PortNumber (getServerPortNumber wd)

-- | stops the server of the worker
stopServer :: MVar WorkerData -> IO (Bool)
stopServer dat
  = do
    workerData <- takeMVar dat
    (r, newServerId) <- case (getServerThreadId workerData) of
      Nothing -> do 
                 putStrLn "no server running"
                 return (False, Nothing)
      Just i  -> do 
                 putStrLn "stopping server"
                 killThread i
                 return (True, Nothing)
    putMVar dat (setServerThreadId newServerId workerData)
    return r

-- | dispatches the server-requests of the worker
requestDispatcher :: MVar WorkerData -> Handle -> HostName -> PortNumber -> IO ()
requestDispatcher dat hdl _ _
 = do
   c <- getCommand hdl  
   res <- dispatch (getCommandString c) c
   return res
   where 
     dispatch cmd c | cmd == pingCmd      = handleHeartBeat hdl c
                    | cmd == startTaskCmd = handleStartTask dat hdl c
                    | otherwise           = handleUnknownCommand hdl cmd c
   
handleHeartBeat :: Handle -> Command -> IO ()
handleHeartBeat hdl _
  = do
    putStrLn "ping-Command"
    putCommand pingresp hdl
    where
      pingresp = mkCommand successCode

handleStartTask :: MVar WorkerData -> Handle -> Command -> IO ()
handleStartTask dat hdl cmd
  = do
    putStrLn "startTask-Command"
    task <- return (getTaskDataParameter "TASK" cmd)
    case task of
      (Just t) ->
        do
        wd <- readMVar dat
        success <- (processTask (getTaskLookUpMap wd) t)
        if (success) 
          then
            do
            putCommand (mkCommand successCode) hdl
          else
            do
            putCommand (mkCommand failureCode) hdl
      _ ->
        do
        putStrLn "invalid task parameter found"
        putCommand (mkCommand failureCode) hdl
          
handleUnknownCommand :: Handle -> String -> Command -> IO()
handleUnknownCommand hdl cmd _ 
  = do
    putStrLn ("unknownCommand: " ++ cmd)    
    putCommand negresp hdl
    where
      negresp = addCommandParameter "MSG" (encode "unknown command") $ mkCommand failureCode
      
{-
-- MR - testcase start
-- -----------------------------------------------------------------------

doStartMapReduce :: IO ()
doStartMapReduce
  = do
    r <- mapReduce 1 mapFkt redFkt inputFkt
    putStrLn (show r)
    putStrLn "starting with MR"

mapFkt :: (Int -> String -> IO [(String, Int)])
mapFkt _ v 
  = do
    return (zip (words v) ones)
    where
      ones = 1 : ones

redFkt :: (String -> [Int] -> IO (Maybe Int))
redFkt _ v = return (Just (sum v))

inputFkt :: [(Int, String)]
inputFkt = [(1, "hallo welt"), (2, "hallo john"), (3, "hallo john")]


-- MR - testcase end
-- -----------------------------------------------------------------------
-}