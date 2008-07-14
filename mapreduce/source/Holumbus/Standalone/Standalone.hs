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

, addJob
, doSingleStep
  
, printDebug
)
where

import Control.Concurrent

import qualified Holumbus.FileSystem.FileSystem as FS

import Holumbus.MapReduce.Types
import Holumbus.MapReduce.JobController
import Holumbus.MapReduce.TaskProcessor




-- ----------------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------------


data StandaloneData = StandaloneData {
    sad_JobController :: JobController
  , sad_TaskProcessor :: TaskProcessor
  }

type Standalone = MVar StandaloneData


sendStartTask :: TaskProcessor -> TaskData -> IO (TaskSendResult)
sendStartTask tp td
  = do
    putStrLn "starting Task"
    putStrLn "TaskData:"
    putStrLn $ show td
    startTask td tp
    return TSRSend

sendTaskCompleted :: JobController -> TaskData -> IO Bool
sendTaskCompleted jc td
  = do
    putStrLn "Task completed"
    putStrLn "TaskData:"
    putStrLn $ show td
    setTaskCompleted jc td
    return True


sendTaskError :: JobController -> TaskData -> IO Bool
sendTaskError jc td
  = do
    putStrLn "Task error"
    putStrLn "TaskData:"
    putStrLn $ show td
    setTaskError jc td
    return True



-- ----------------------------------------------------------------------------
-- Creation and Initialisation
-- ----------------------------------------------------------------------------


newStandalone :: MapFunctionMap -> ReduceFunctionMap-> IO Standalone
newStandalone mm rm
  = do
    -- get a new JobController an TaskProcessor
    jc <- newJobController
    tp <- newTaskProcessor
    
    -- configure the JobController
    setTaskSendHook (sendStartTask tp) jc
    
    -- configure the TaskProcessor
    setMapFunctionMap mm tp
    setReduceFunctionMap rm tp
    setTaskCompletedHook (sendTaskCompleted jc) tp
    setTaskErrorHook (sendTaskError jc) tp
     
    -- startJobController jc
    startTaskProcessor tp
    
    let sad = StandaloneData jc tp
    newMVar sad 


closeStandalone :: Standalone -> IO ()
closeStandalone sa
  = do
    modifyMVar sa $
      \sad ->
      do
      closeTaskProcessor (sad_TaskProcessor sad)
      closeJobController (sad_JobController sad)
      return (sad, ())
      


addJob :: JobInfo -> Standalone -> IO ()
addJob ji sa
  = do
    modifyMVar sa $
      \sad ->
      do
      startJob ji (sad_JobController sad)
      return (sad, ())


doSingleStep :: Standalone -> IO ()
doSingleStep sa
  = do
    modifyMVar sa $
      \sad ->
      do
      putStrLn "doSingleStep"
      singleStepJobControlling (sad_JobController sad)
      return (sad, ())
      

printDebug :: Standalone -> IO ()
printDebug sa
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
      mapFuns <- getMapFunctions (sad_TaskProcessor sad)
      putStrLn $ show $ mapFuns
      putStrLn "--------------------------------------------------------"
      putStrLn "Reduce-Functions"
      reduceFuns <- getReduceFunctions (sad_TaskProcessor sad)
      putStrLn $ show $ reduceFuns
      putStrLn "--------------------------------------------------------"