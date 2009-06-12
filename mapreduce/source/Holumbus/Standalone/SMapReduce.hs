-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Standalone.SMapReduce
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.Standalone.SMapReduce
(
-- * Datatypes
  SMapReduce
, MapReduce(..)

-- * Configurations
, SMRConf
, defaultStandaloneConfig
  
-- * Creation and Initialisation
, newSMapReduce
)
where

import           Control.Concurrent
import           System.Log.Logger

import           Holumbus.Common.Debug
import qualified Holumbus.FileSystem.FileSystem as FS
import           Holumbus.MapReduce.Types
import           Holumbus.MapReduce.JobController
import           Holumbus.MapReduce.TaskProcessor
import           Holumbus.MapReduce.MapReduce
import           Holumbus.Network.Site


localLogger :: String
localLogger = "Holumbus.Standalone.SMapReduce"

-- ----------------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------------


data SMapReduceData = SMapReduceData {
    sad_JobController :: JobController
  , sad_TaskProcessor :: TaskProcessor
  }


data SMapReduce = SMapReduce (MVar SMapReduceData)

instance Show SMapReduce where
  show _ = "SMapReduce"



-- ---------------------------------------------------------------------------
-- Configurations
-- ---------------------------------------------------------------------------


data SMRConf = SMRConf {
   stc_StartControlling :: Bool
}

defaultStandaloneConfig :: SMRConf
defaultStandaloneConfig = SMRConf True



-- ----------------------------------------------------------------------------
-- "glue" between JobController and TaskProcessor
-- ----------------------------------------------------------------------------


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


newSMapReduce 
  :: FS.FileSystem -> ActionMap -> SMRConf
  -> IO SMapReduce
newSMapReduce fs am conf
  = do
    let start = stc_StartControlling conf
    -- get a new JobController an TaskProcessor
    jc <- newJobController
    tp <- newTaskProcessor
    
    -- configure the JobController
    setTaskSendHook (sendStartTask tp) jc
    
    -- configure the TaskProcessor
    setFileSystemToTaskProcessor fs tp
    setActionMap am tp
    setTaskCompletedHook (sendTaskCompleted jc) tp
    setTaskErrorHook (sendTaskError jc) tp
     
    if (start)
      then do startJobController jc
      else do return ()
    
    startTaskProcessor tp
    
    let sad = SMapReduceData jc tp
    sa <- (newMVar sad)
    return (SMapReduce sa) 

      
{-      
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
      decodeResult' (TupleFunctionData b) = decodeTuple b
-}



-- ----------------------------------------------------------------------------
-- Typeclass instanciation
-- ----------------------------------------------------------------------------

instance Debug SMapReduce where

  printDebug (SMapReduce sa)
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
        putStrLn "Actions"
        actions <- getActions (sad_TaskProcessor sad)
        putStrLn $ show $ actions

  getDebug (SMapReduce sa)
    = do
      withMVar sa $
        \sad ->
        do
        showJC <- printJobController (sad_JobController sad)
        showTP <- printTaskProcessor (sad_TaskProcessor sad)
        actions <- getActions (sad_TaskProcessor sad)
        let line= "--------------------------------------------------------"
        return (line
          ++"\n"++ "Job-Controller"
          ++"\n"++ showJC
          ++"\n"++ line
          ++"\n"++ "Task-Processor"
          ++"\n"++ showTP
          ++"\n"++ line
          ++"\n"++ "Actions"
          ++"\n"++ show actions++"\n")



instance MapReduce SMapReduce where


  closeMapReduce (SMapReduce sa)
    = modifyMVar sa $
        \sad ->
        do
        closeTaskProcessor (sad_TaskProcessor sad)
        closeJobController (sad_JobController sad)
        return (sad, ())


  getMySiteId _
    = getSiteId

  
  getMapReduceType _
    = return MRTStandalone

  
  startControlling (SMapReduce sa)
    = withMVar sa $
        \sad -> do startJobController (sad_JobController sad)
  
  
  
  stopControlling (SMapReduce sa)
    = withMVar sa $
        \sad -> do stopJobController (sad_JobController sad)
  

  
  isControlling (SMapReduce sa)
    = withMVar sa $
        \sad -> do isJobControllerRunning (sad_JobController sad)
  

    
  doSingleStep (SMapReduce sa)
    = withMVar sa $
        \sad -> do singleStepJobControlling (sad_JobController sad)

  
  doMapReduceJob ji (SMapReduce sa)
    = withMVar sa $
        \sad -> do performJob ji (sad_JobController sad)
