
module Main (main) where

import Control.Concurrent

import System.IO
import System.Log.Logger

import Holumbus.Common.Logging
import Holumbus.Console.Console
import Holumbus.Master.MasterData
import Holumbus.Master.MRMasterServer
import Holumbus.Master.HeartBeat
import qualified Holumbus.Master.ProcessScheduler as PS
import Holumbus.Task.TaskData
import Holumbus.Task.DemoTasks

localLogger :: String
localLogger = "Main"

version :: String
version = "masterMR 0.1"

main :: IO ()
main
  = do
    -- we use hsLogging and have our own configuration
    initializeLogging
    emptyData <- (initializeMasterData demoTaskNameSet)
    masterData <- newMVar emptyData
    debugM localLogger "program started"    
--args <- getArgs
--    case args of 
--        [_]  -> listenForRequests key master 4242
--        _       -> usage
    handleUserInput createConsole masterData


createConsole :: ConsoleData (MVar MasterData)
createConsole = 
  addConsoleCommand "debug" printDebug "prints internal state of master" $ 
  addConsoleCommand "stopServer" doStopServer "stops master server" $ 
  addConsoleCommand "startServer" doStartServer "starts master server" $
  addConsoleCommand "stopHeartBeat" doStopHeartBeat "stops the heartbeat thread" $ 
  addConsoleCommand "startHeartBeat" doStartHeartBeat "starts the heartbeat thread" $
  addConsoleCommand "startTask" doStartTask "start demo task" $ 
  addConsoleCommand "stopTask" doStopTask "stop demo task" $
  addConsoleCommand "startScheduler" doStartScheduler "starts the scheduler" $
  addConsoleCommand "stopScheduler" doStopScheduler "stops the scheduler" $
  addConsoleCommand "version" printVersion "prints the version" $ 
  addConsoleCommand "status" printStatus "prints the internal status" $
  initializeConsole

doStartHeartBeat :: MVar MasterData -> [String] -> IO ()
doStartHeartBeat masterData _
  = do
    startHeartBeat masterData
    return ()

doStopHeartBeat :: MVar MasterData -> [String] -> IO ()
doStopHeartBeat masterData _
  = do
    stopHeartBeat masterData
    return ()

doStartServer :: MVar MasterData -> [String] -> IO ()
doStartServer masterData _
  = do
    startServer masterData
    return ()

doStopServer :: MVar MasterData -> [String] -> IO ()
doStopServer masterData _
  = do
    stopServer masterData
    return ()

doStartScheduler :: MVar MasterData -> [String] -> IO ()
doStartScheduler masterData _
  = do
    md <- readMVar masterData
    PS.startScheduler (getProcessScheduler md)
    return ()

doStopScheduler :: MVar MasterData -> [String] -> IO ()
doStopScheduler masterData _
  = do
    md <- readMVar masterData
    PS.stopScheduler (getProcessScheduler  md)
    return ()

doStartTask :: MVar MasterData -> [String] -> IO ()
doStartTask masterData _
  = do
    md <- readMVar masterData
    ps <- takeMVar (getProcessScheduler md)
    (idx, newPs) <- return (PS.addTask ( mkNewTask "WORDCOUNT" [] ) ps)
    putMVar (getProcessScheduler md) newPs
    putStrLn ("added new Task with id: " ++ show idx)
    return ()
    
doStopTask :: MVar MasterData -> [String] -> IO ()
doStopTask _ _
  = do
    putStrLn "not implemented"
    return ()

printStatus :: MVar MasterData -> [String] -> IO ()
printStatus masterData _
  = do
    return ()
    md <- readMVar masterData
    ps <- readMVar (getProcessScheduler md)
    putStrLn (show ps)

printDebug :: MVar MasterData -> [String] -> IO ()
printDebug masterData _
  = do
    md <- readMVar masterData    
    printMasterData md

printVersion :: MVar MasterData -> [String] -> IO ()
printVersion _ _ 
  = do
    putStrLn version
