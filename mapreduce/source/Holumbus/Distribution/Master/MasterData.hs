-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Distribution.Master.MasterData
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-binds #-} -- for unused record field selectors

module Holumbus.Distribution.Master.MasterData
    (
      -- * Datatypes
      MasterData
  
      -- * creation and destruction
    , newMaster
    )
where

import           Control.Concurrent

import           Data.List
import           Data.Maybe
import qualified Data.Set                                       as Set

import           System.Log.Logger

import           Holumbus.Common.Debug
import           Holumbus.Common.Utils

import           Holumbus.Network.Site
-- import           Holumbus.Network.Port
-- import           Holumbus.Network.Messages
import           Holumbus.Network.Communication

import qualified Holumbus.MapReduce.MapReduce                   as MR
import           Holumbus.MapReduce.JobController
import           Holumbus.MapReduce.Types

import qualified Holumbus.Data.MultiMap                         as MMap
import qualified Holumbus.FileSystem.FileSystem                 as FS

import qualified Holumbus.Distribution.Messages                 as M
import qualified Holumbus.Distribution.Master                   as MC
import qualified Holumbus.Distribution.Worker                   as WC
import qualified Holumbus.Distribution.Worker.WorkerPort        as WP
import           Holumbus.Distribution.Master

localLogger :: String
localLogger = "Holumbus.Distribution.Master.MasterData"

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

type TaskToWorkerMap = MMap.MultiMap TaskId M.WorkerId
type WorkerToTaskMap = MMap.MultiMap M.WorkerId TaskId

type ActionToWorkerMap = MMap.MultiMap ActionName M.WorkerId


data WorkerControllerData = WorkerControllerData {
    wcd_TaskToWorkerMap   :: ! TaskToWorkerMap
  , wcd_WorkerToTaskMap   :: ! WorkerToTaskMap
  , wcd_ActionToWorkerMap :: ! ActionToWorkerMap
  }

type WorkerController = MVar WorkerControllerData

data MasterData = MasterData {
    md_Server          :: Server
  , md_WorkerController ::   WorkerController
  , md_JobController    ::   JobController
  , md_FileSystem       ::   FS.FileSystem
  }



-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


newWorkerController :: IO WorkerController
newWorkerController
  = do
    let wcd = WorkerControllerData
                MMap.empty
                MMap.empty
                MMap.empty
    newMVar wcd


newMaster
  :: FS.FileSystem
  -> Bool -> StreamName -> Maybe PortNumber
  -> IO MasterData
newMaster fs start sn pn
  = do
    -- initialise the server
    m <- newEmptyMVar
    server <- newServer sn pn (dispatch m) (Just $ registerWorker m) (Just $ unregisterWorker m)
    
    -- start the WorkerController
    wc <- newWorkerController

    -- start the JobController
    jc <- newJobController
    -- configure the JobController
    setTaskSendHook (sendStartTask server wc) jc
    if (start) 
      then do startJobController jc
      else do return ()

    -- get the internal data
    let md = MasterData server wc jc fs
    
    -- get the internal data
    putMVar m md
    
    return md

  
dispatch 
  :: MVar MasterData 
  -> M.MasterRequestMessage 
  -> IO (Maybe M.MasterResponseMessage)
dispatch m msg
  = do
    md <- readMVar m
    case msg of
      (M.MReqTaskCompleted td) ->
        do
        MC.receiveTaskCompleted td md
        return $ Just $ M.MRspSuccess
      (M.MReqTaskError td) ->
        do
        MC.receiveTaskError td md
        return $ Just $ M.MRspSuccess
      (M.MReqStartControlling) ->
        do
        MR.startControlling md
        return $ Just $ M.MRspSuccess         
      (M.MReqStopControlling) ->
        do
        MR.stopControlling md
        return $ Just $ M.MRspSuccess         
      (M.MReqIsControlling) ->
        do
        MR.isControlling md
        return $ Just $ M.MRspSuccess         
      (M.MReqSingleStep) ->
        do
        MR.doSingleStep md
        return $ Just $ M.MRspSuccess
      (M.MReqPerformJob ji) ->
        do
        r <- MR.doMapReduceJob ji md
        return $ Just $ M.MRspResult r
      _ -> return Nothing



registerWorker :: MVar MasterData -> IdType -> ClientPort -> IO ()
registerWorker m i cp
  = do
    let wp = WP.newWorkerPort cp
    as <- WC.getActionNames wp
    md <- readMVar m
    modifyMVar (md_WorkerController md) $
      \wcd ->
      do
      let wcd' = addWorkerToMaster i as wcd
      return (wcd',())
    
    


unregisterWorker :: MVar MasterData -> IdType -> ClientPort -> IO ()
unregisterWorker m i _
  = do
    md <- readMVar m
    modifyMVar (md_WorkerController md) $
      \wcd ->
      do
      -- TODO delete Tasks from Worker
      let wcd' = deleteWorkerFromMaster i wcd      
      return (wcd',())
    
    return ()


-- ----------------------------------------------------------------------------
-- private functions
-- ----------------------------------------------------------------------------

addWorkerToMaster
  :: M.WorkerId -> [ActionName]
  -> WorkerControllerData -> WorkerControllerData
addWorkerToMaster wid as wcd
  = wcd { wcd_ActionToWorkerMap = awm' }
  where
  -- update the ActionToWorkerMap
  awm' = MMap.insertKeys as (Set.singleton wid) $ wcd_ActionToWorkerMap wcd
        

deleteWorkerFromMaster :: M.WorkerId -> WorkerControllerData -> WorkerControllerData
deleteWorkerFromMaster wid wcd
  = wcd { wcd_ActionToWorkerMap = awm' }
  where
  -- update the ActionToWorkerMap 
  awm' = MMap.deleteAllElems wid $ wcd_ActionToWorkerMap wcd


getTasksPerWorkerWithAction :: ActionName -> WorkerControllerData -> [(Int, M.WorkerId)]
getTasksPerWorkerWithAction a wcd = nullList ++ sortedList
  where
  wtm = wcd_WorkerToTaskMap wcd
  -- all Workers with the needed Action
  allWids = MMap.lookup a $ wcd_ActionToWorkerMap wcd
  -- all WorkerIds with Tasks and the Action
  taskWids = Set.intersection allWids $ MMap.keys wtm
  -- all WorkerIds without Tasks
  noTaskWids = Set.difference allWids taskWids
  -- list with all Workers with no tasks
  nullList = map (\wid -> (0,wid)) (Set.toList noTaskWids)
  -- list with all Workers with their tasks
  ls2 = map (\(wid,s) -> (Set.size s, wid)) (MMap.toList wtm)
  -- merging and sorting the list
  sortedList = sortBy (\(n1,_) (n2,_) -> compare n1 n2) ls2
  

addTaskToWorker :: TaskId -> M.WorkerId -> WorkerControllerData -> WorkerControllerData
addTaskToWorker tid wid wcd = wcd { wcd_TaskToWorkerMap = twm',  wcd_WorkerToTaskMap = wtm'}
  where
  twm' = MMap.insert tid wid (wcd_TaskToWorkerMap wcd)
  wtm' = MMap.insert wid tid (wcd_WorkerToTaskMap wcd)
  

-- TODO use this
deleteTaskFromWorker :: TaskId -> M.WorkerId -> WorkerControllerData -> WorkerControllerData
deleteTaskFromWorker tid wid wcd = wcd { wcd_TaskToWorkerMap = twm',  wcd_WorkerToTaskMap = wtm'}
  where
  twm' = MMap.deleteElem tid wid (wcd_TaskToWorkerMap wcd)
  wtm' = MMap.deleteElem wid tid (wcd_WorkerToTaskMap wcd)

-- TODO use this
deleteTaskFromWorkers :: TaskId -> WorkerControllerData -> WorkerControllerData
deleteTaskFromWorkers tid wcd = wcd''
  where
  wids = Set.toList $ MMap.lookup tid (wcd_TaskToWorkerMap wcd)
  wcd'' = foldl (\wcd' wid -> deleteTaskFromWorker tid wid wcd') wcd wids


sendStartTask :: Server -> WorkerController -> TaskData -> IO (TaskSendResult)
sendStartTask s wc td
  = handleAll ( \ e -> do 
		       errorM localLogger $ "sendStartTask: " ++ show e
		       return TSRError
	      ) $
      do
      debugM localLogger $ "sendStartTask: waiting for wc"
      modifyMVar wc $
        \wcd ->
        do
        debugM localLogger $ "sendStartTask: waiting for wc -> done"
        -- get all workers with the required action, sorted by the number of current tasks 
        let wls = getTasksPerWorkerWithAction (td_Action td) wcd
        if (null wls)
          then do
            warningM localLogger $ "sendStartTask: no worker with action \"" ++ (td_Action td) ++ "\"found"
            return (wcd, TSRNotSend)
          else do          
            infoM localLogger $ "starting Task: " ++ show (td_TaskId td)
            debugM localLogger $ "sendStartTask: wls:" ++ show wls
            let (_,wid) = head wls
            info <- getClientInfo wid s
            case info of
              (Just ci) ->
                do
                WC.startTask td $ WP.newWorkerPort (ci_Port ci)
                let wcd' = addTaskToWorker (td_TaskId td) wid wcd
                return (wcd', TSRSend)                
              (Nothing) ->
                return (wcd, TSRNotSend)

-- ----------------------------------------------------------------------------
-- typeclass instanciation
-- ----------------------------------------------------------------------------

instance MR.MapReduce MasterData where

  closeMapReduce md
    = closeMaster md

  getMySiteId _ 
    = getSiteId

  
  getMapReduceType _
    = return MR.MRTMaster

  
  startControlling md 
    = do
      debugM localLogger "startControlling"
      startJobController (md_JobController md)


  stopControlling md 
    = do
      debugM localLogger "stopControlling"
      stopJobController (md_JobController md)


  isControlling md
    = do
      debugM localLogger "isControlling"
      isJobControllerRunning (md_JobController md)

  
  doSingleStep md
    = do
      debugM localLogger "doSingleStep"
      singleStepJobControlling (md_JobController md)

  
  doMapReduceJob ji md
    = do
      debugM localLogger "doMapReduceJob"
      performJob ji (md_JobController md)




instance MasterClass MasterData where


  closeMaster md 
    = do
      closeServer (md_Server md)
      return ()


  receiveTaskCompleted td md
    = do
      debugM localLogger $ "completed Task: " ++ show (td_TaskId td)
      setTaskCompleted (md_JobController md) td
      -- TODO inform other workers
      -- TODO delete from workerController
      return md


  receiveTaskError td md
    = do
      debugM localLogger $ "error Task: " ++ show (td_TaskId td)
      setTaskError (md_JobController md) td
      -- TODO inform other workers
      -- TODO delete from workerController
      return md
      
      


instance Debug MasterData where
  printDebug md
    = do
      putStrLn "Master-Object (full)"
      putStrLn "--------------------------------------------------------"        
      putStrLn "Server"
      printDebug (md_Server md)
      withMVar (md_WorkerController md) $
        \wcd -> 
        do
        putStrLn $ prettyRecordLine gap "TaskToWorkerMap: " (wcd_TaskToWorkerMap wcd)
        putStrLn $ prettyRecordLine gap "WorkerToTaskMap" (wcd_WorkerToTaskMap wcd)
        putStrLn $ prettyRecordLine gap "ActionToWorkerMap:" (wcd_ActionToWorkerMap wcd)        
        putStrLn $ "JobController:"
        jc <- printJobController (md_JobController md)
        putStrLn jc
      where
        gap = 20
      
