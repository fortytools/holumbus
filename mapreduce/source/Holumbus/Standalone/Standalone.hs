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
    sad_fileSystem :: FS.FileSystem
  , sad_controller :: JobControlData
  , sad_processor  :: TaskProcessor
  }

type Standalone = MVar StandaloneData


sendTask :: TaskProcessor -> TaskData -> IO (TaskSendResult)
sendTask tp td
  = do
    startTask td tp
    return TSRSend

-- ----------------------------------------------------------------------------
-- Creation and Initialisation
-- ----------------------------------------------------------------------------


newStandalone :: FS.FileSystem -> MapFunctionMap -> ReduceFunctionMap-> IO Standalone
newStandalone fs mm rm
  = do
    tp <- newTaskProcessor
    setMapFunctionMap mm tp
    setReduceFunctionMap rm tp
    let jcd = newJobControlData (sendTask tp)
    let sad = StandaloneData fs jcd tp
    newMVar sad 

closeStandalone :: Standalone -> IO ()
closeStandalone _
  = do
    return ()


addJob :: JobInfo -> Standalone -> IO ()
addJob ji sa
  = do
    modifyMVar sa $
      \sad ->
      do
      jc' <- createNewJob ji (sad_controller sad)
      let sad' = sad { sad_controller = jc' }
      return (sad', ())

doSingleStep :: Standalone -> IO ()
doSingleStep sa
  = do
    modifyMVar sa $
      \sad ->
      do
      jc' <- doControlling (sad_controller sad)
      let sad' = sad { sad_controller = jc' }
      return (sad', ())
      

printDebug :: Standalone -> IO ()
printDebug sa
  = do
    withMVar sa $
      \sad ->
      do
      putStrLn "--------------------------------------------------------"      
      putStrLn "Job-Controller"
      putStrLn $ show (sad_controller sad)
      putStrLn "--------------------------------------------------------"
      putStrLn "Map-Functions"
      mapFuns <- getMapFunctions (sad_processor sad)
      putStrLn $ show $ mapFuns
      putStrLn "--------------------------------------------------------"
      putStrLn "Reduce-Functions"
      reduceFuns <- getReduceFunctions (sad_processor sad)
      putStrLn $ show $ reduceFuns
      putStrLn "--------------------------------------------------------"