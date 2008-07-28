-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Standalone.Standalone
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.Standalone.Standalone
(
-- * Datatypes
  Standalone
  
-- * Creation and Initialisation
, newStandalone
, closeStandalone
)
where

import           Control.Concurrent
import           System.Log.Logger

import qualified Holumbus.FileSystem.FileSystem as FS
import           Holumbus.MapReduce.Types
import           Holumbus.MapReduce.JobController
import           Holumbus.MapReduce.TaskProcessor
import           Holumbus.MapReduce.MapReduce
import           Holumbus.Network.Site


localLogger :: String
localLogger = "Holumbus.Standalone.Standalone"

-- ----------------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------------


data StandaloneData = StandaloneData {
    sad_JobController :: JobController
  , sad_TaskProcessor :: TaskProcessor
  }


data Standalone = Standalone (MVar StandaloneData)


sendStartTask :: TaskProcessor -> TaskData -> IO (TaskSendResult)
sendStartTask tp td
  = do
    debugM localLogger "sendStartTask"
    debugM localLogger "TaskData:"
    debugM localLogger $ show td
    startTask td tp
    return TSRSend


sendTaskCompleted :: JobController -> TaskData -> IO Bool
sendTaskCompleted jc td
  = do
    debugM localLogger "Task completed"
    debugM localLogger "TaskData:"
    debugM localLogger $ show td
    setTaskCompleted jc td
    return True


sendTaskError :: JobController -> TaskData -> IO Bool
sendTaskError jc td
  = do
    debugM localLogger "Task error"
    debugM localLogger "TaskData:"
    debugM localLogger $ show td
    setTaskError jc td
    return True



-- ----------------------------------------------------------------------------
-- Creation and Initialisation
-- ----------------------------------------------------------------------------


newStandalone :: FS.FileSystem -> MapActionMap -> ReduceActionMap -> IO Standalone
newStandalone fs mm rm
  = do
    -- get a new JobController an TaskProcessor
    jc <- newJobController
    tp <- newTaskProcessor
    
    -- configure the JobController
    setTaskSendHook (sendStartTask tp) jc
    setMapActions mm jc
    setReduceActions rm jc
    
    -- configure the TaskProcessor
    setFileSystemToTaskProcessor fs tp
    setMapActionMap mm tp
    setReduceActionMap rm tp
    setTaskCompletedHook (sendTaskCompleted jc) tp
    setTaskErrorHook (sendTaskError jc) tp
     
    -- startJobController jc
    startTaskProcessor tp
    
    let sad = StandaloneData jc tp
    sa <- (newMVar sad)
    return (Standalone sa) 


closeStandalone :: Standalone -> IO ()
closeStandalone (Standalone sa)
  = do
    modifyMVar sa $
      \sad ->
      do
      closeTaskProcessor (sad_TaskProcessor sad)
      closeJobController (sad_JobController sad)
      return (sad, ())
      
printJobResult :: MVar JobResult -> IO ()
printJobResult mVarRes
  = do
    forkIO $
      withMVar mVarRes $ 
        \(JobResult outs) -> 
        do
        putStrLn "RESULT:" 
        putStrLn $ show (decodeResult outs)
    return ()
    where
    decodeResult :: [FunctionData] -> [(String, Integer)]
    decodeResult ls = map decodeResult' ls
      where
      decodeResult' (FileFunctionData f) = (f, -1)
      decodeResult' (RawFunctionData b) = decodeTuple b




-- ----------------------------------------------------------------------------
-- Typeclass instanciation
-- ----------------------------------------------------------------------------

instance MapReduce Standalone where

  getMySiteId _
    = do
      getSiteId

  addJob ji (Standalone sa)
    = do
      withMVar sa $
        \sad ->
        do
        r <- startJob ji (sad_JobController sad)
        case r of
          (Left m) -> putStrLn m
          (Right (_,res)) -> printJobResult res
        return ()


  doSingleStep (Standalone sa)
    = do
      withMVar sa $
        \sad ->
        do
        putStrLn "doSingleStep"
        singleStepJobControlling (sad_JobController sad)
        return ()
      

  printDebug (Standalone sa)
    = do
      withMVar sa $
        \sad ->
        do
        putStrLn "--------------------------------------------------------"      
        putStrLn "Job-Controller"
        showJC <- printJobController (sad_JobController sad)
        putStrLn showJC
        putStrLn "--------------------------------------------------------"
        putStrLn "Task-Processor"
        showTP <- printTaskProcessor (sad_TaskProcessor sad)
        putStrLn $showTP
        putStrLn "--------------------------------------------------------"
        putStrLn "Map-Functions"
        mapFuns <- getMapActions (sad_TaskProcessor sad)
        putStrLn $ show $ mapFuns
        putStrLn "--------------------------------------------------------"
        putStrLn "Reduce-Functions"
        reduceFuns <- getReduceActions (sad_TaskProcessor sad)
        putStrLn $ show $ reduceFuns
        putStrLn "--------------------------------------------------------"