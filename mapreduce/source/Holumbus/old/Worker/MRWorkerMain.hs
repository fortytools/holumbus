
module Main (main) where

import System.IO
import System.Log.Logger
 
import Control.Concurrent
import Control.Monad

import Holumbus.Common.Logging
import Holumbus.Console.Console
import Holumbus.Task.DemoTasks
import Holumbus.Task.TaskData
import Holumbus.Task.TaskProcessor
import Holumbus.Worker.MRWorkerClient
import Holumbus.Worker.MRWorkerServer
import Holumbus.Worker.WorkerData

--import Holumbus.Control.MapReduce.Parallel

localLogger :: String
localLogger = "Main"

version :: String
version = "MRWorker 0.1"

main :: IO ()
main 
  = do
    -- we use hsLogging and have our own configuration
    initializeLogging
    workerData <- newMVar (initializeWorkerData demoTaskLookUpMap) 
    debugM localLogger "program started"  
    handleUserInput createConsole workerData
    debugM localLogger "programm finished"

createConsole :: ConsoleData (MVar WorkerData)
createConsole = 
  addConsoleCommand "debug" printDebug "prints internal state of worker" $ 
  addConsoleCommand "stopServer" doStopServer "stops worker server" $ 
  addConsoleCommand "startServer" doStartServer "starts worker server" $ 
  addConsoleCommand "register" doRegister "registers Worker at Master" $ 
  addConsoleCommand "unregister" doUnregister "unregisters worker at master" $
  addConsoleCommand "startTask" doStartTask "start a task - only for debug" $
  addConsoleCommand "version" printVersion "prints the version" $ 
  initializeConsole

doRegister :: MVar WorkerData -> [String] -> IO ()
doRegister workerData _
  = do
    registerWorker workerData
    return ()

doUnregister :: MVar WorkerData -> [String] -> IO ()
doUnregister workerData _
  = do
    unregisterWorker workerData
    return ()

doStartServer :: MVar WorkerData -> [String] -> IO ()
doStartServer workerData _
  = do
    startServer workerData
    return ()

doStopServer :: MVar WorkerData -> [String] -> IO ()
doStopServer workerData _
  = do
    stopServer workerData
    return ()

doStartTask :: MVar WorkerData -> [String] -> IO ()
doStartTask workerData opts
  = do
    if (null opts)
      then
        do
        putStrLn "no task given"
        return ()
      else
        do
        wd <- readMVar workerData
        success <- processTask (getTaskLookUpMap wd) (setTaskId 1 $ mkNewTask (head opts) (tail opts))
        if (success) then putStrLn "success" else putStrLn "failure"
        return ()

printDebug :: MVar WorkerData -> [String] -> IO ()
printDebug workerData _
  = do
    wd <- readMVar workerData
    printWorkerData wd

printVersion :: MVar WorkerData -> [String] -> IO ()
printVersion _ _
  = do
    putStrLn version
