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

module Holumbus.Distribution.Master.MasterData
(
-- * Datatypes
  MasterData
  
-- * creation and destruction
, newMaster
)
where


import           Control.Concurrent
import qualified Control.Exception as E
import           Data.List
import           Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import           System.Log.Logger

import           Holumbus.Common.Debug
import           Holumbus.Network.Site
import           Holumbus.Network.Port
import           Holumbus.Network.Messages
import qualified Holumbus.MapReduce.MapReduce as MR
import           Holumbus.MapReduce.JobController
import           Holumbus.MapReduce.Types
import qualified Holumbus.Data.MultiMap as MMap
import qualified Holumbus.FileSystem.FileSystem as FS
import qualified Holumbus.Distribution.Messages as M
import qualified Holumbus.Distribution.Master as MC
import qualified Holumbus.Distribution.Worker as WC
import qualified Holumbus.Distribution.Worker.WorkerPort as WP
import           Holumbus.Distribution.Master
import           Holumbus.Common.Utils


localLogger :: String
localLogger = "Holumbus.Distribution.Master.MasterData"



-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


type SiteData = (SiteId, WP.WorkerPort, [ActionName])

type WorkerToSiteMap = Map.Map M.WorkerId SiteData
type SiteToWorkerMap = MMap.MultiMap SiteId M.WorkerId

type JobMap = Map.Map JobId JobData

type TaskToWorkerMap = MMap.MultiMap TaskId M.WorkerId
type WorkerToTaskMap = MMap.MultiMap M.WorkerId TaskId

type ActionToWorkerMap = MMap.MultiMap ActionName M.WorkerId


data WorkerControllerData = WorkerControllerData {
    wcd_WorkerToSiteMap   :: ! WorkerToSiteMap
  , wcd_SiteToWorkerMap   :: ! SiteToWorkerMap
  , wcd_SiteMap           :: ! SiteMap
  , wcd_WorkerId          :: ! M.WorkerId
  , wcd_JobMap            :: ! JobMap
  , wcd_TaskToWorkerMap   :: ! TaskToWorkerMap
  , wcd_WorkerToTaskMap   :: ! WorkerToTaskMap
  , wcd_ActionToWorkerMap :: ! ActionToWorkerMap
  }

type WorkerController = MVar WorkerControllerData

data MasterData = MasterData {
    md_ServerThreadId   ::   MVar (Maybe ThreadId)
  , md_OwnStream        :: ! M.MasterRequestStream
  , md_OwnPort          :: ! M.MasterRequestPort
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
                Map.empty
                MMap.empty
                emptySiteMap
                0
                Map.empty
                MMap.empty
                MMap.empty
                MMap.empty
    wc <- newMVar wcd
    return wc


newMaster
  :: FS.FileSystem
  -> Bool -> StreamName
  -> IO MasterData
newMaster fs start sn
  = do
    -- initialize values 
    st    <- (newGlobalStream sn::IO M.MasterRequestStream)
    po    <- newPortFromStream st

    -- we can't start the server yet
    tid   <- newMVar Nothing

    -- start the WorkerController
    wc <- newWorkerController

    -- start the JobController
    jc <- newJobController
    -- configure the JobController
    setTaskSendHook (sendStartTask wc) jc
    
    if (start) 
      then do startJobController jc
      else do return ()

    -- get the internal data
    let md = MasterData tid st po wc jc fs
    startRequestDispatcher tid  st (dispatch md)
    return md

  
dispatch 
  :: MasterData 
  -> M.MasterRequestMessage 
  -> M.MasterResponsePort
  -> IO ()
dispatch md msg replyPort
  = do
    case msg of
      (M.MReqRegister s p as) ->
        do
        handleRequest replyPort (MC.registerWorker s p as md) (\(nid, _) -> M.MRspRegister nid)
        return ()
      (M.MReqUnregister n) ->
        do
        handleRequest replyPort (MC.unregisterWorker n md) (\_ -> M.MRspUnregister)
        return ()
      (M.MReqTaskCompleted td) ->
        do
        handleRequest replyPort (MC.receiveTaskCompleted td md) (\_ -> M.MRspSuccess)
        return ()
      (M.MReqTaskError td) ->
        do
        handleRequest replyPort (MC.receiveTaskError td md) (\_ -> M.MRspSuccess)
        return ()
      (M.MReqStartControlling) ->
        do
        handleRequest replyPort (MR.startControlling md) (\_ -> M.MRspSuccess)
        return ()         
      (M.MReqStopControlling) ->
        do
        handleRequest replyPort (MR.stopControlling md) (\_ -> M.MRspSuccess)
        return ()         
      (M.MReqIsControlling) ->
        do
        handleRequest replyPort (MR.isControlling md) (\b -> M.MRspIsControlling b)
        return ()         
      (M.MReqSingleStep) ->
        do
        handleRequest replyPort (MR.doSingleStep md) (\_ -> M.MRspSuccess)
        return ()
      (M.MReqPerformJob ji) ->
        do
        handleRequest replyPort (MR.doMapReduce ji md) (\r -> M.MRspResult r)
        return ()
      _ -> handleRequest replyPort (return ()) (\_ -> M.MRspUnknown)



-- ----------------------------------------------------------------------------
-- private functions
-- ----------------------------------------------------------------------------


getNextWorkerId :: WorkerControllerData -> (M.WorkerId, WorkerControllerData)
getNextWorkerId wcd
  = (wid, wcd { wcd_WorkerId = wid })
  where
    wid = (wcd_WorkerId wcd) + 1


lookupWorkerSiteId :: M.WorkerId -> WorkerControllerData -> Maybe SiteId
lookupWorkerSiteId wid wcd = convertSite sd
  where 
    sd = Map.lookup wid $ wcd_WorkerToSiteMap wcd 
    convertSite Nothing = Nothing
    convertSite (Just (sid, _, _)) = Just sid


lookupWorkerPort :: M.WorkerId -> WorkerControllerData -> Maybe WP.WorkerPort
lookupWorkerPort wid wcd = getPort sd
  where 
    sd = Map.lookup wid $ wcd_WorkerToSiteMap wcd
    getPort Nothing = Nothing
    getPort (Just (_, wp, _)) = Just wp


lookupWorkerActions :: M.WorkerId -> WorkerControllerData -> Maybe [ActionName]
lookupWorkerActions wid wcd = getActions sd
  where 
    sd = Map.lookup wid $ wcd_WorkerToSiteMap wcd
    getActions Nothing = Nothing
    getActions (Just (_, _, as)) = Just as


addWorkerToMaster
  :: M.WorkerId -> SiteId -> WP.WorkerPort -> [ActionName]
  -> WorkerControllerData -> WorkerControllerData
addWorkerToMaster wid sid wp as wcd
  = wcd 
      { wcd_WorkerToSiteMap = wsm'
      , wcd_SiteToWorkerMap = swm'
      , wcd_SiteMap = sm' 
      , wcd_ActionToWorkerMap = awm' }
  where
    --update the nodetosite map
    wsm' = Map.insert wid (sid, wp, as) $ wcd_WorkerToSiteMap wcd
    --update the sitetonode map
    swm' = MMap.insert sid wid $ wcd_SiteToWorkerMap wcd
    -- update the SiteMap
    sm' = addIdToMap sid $ wcd_SiteMap wcd
    -- update the ActionToWorkerMap
    awm' = MMap.insertKeys as (Set.singleton wid) $ wcd_ActionToWorkerMap wcd
        

deleteWorkerFromMaster :: M.WorkerId -> WorkerControllerData -> WorkerControllerData
deleteWorkerFromMaster wid wcd
  = wcd 
      { wcd_WorkerToSiteMap = wsm'
      , wcd_SiteToWorkerMap = swm'
      , wcd_SiteMap = sm'
      , wcd_ActionToWorkerMap = awm' }
  where
    -- update the nodetosite map 
    wsm' = Map.delete wid $ wcd_WorkerToSiteMap wcd 
    -- update the sitetonode and the siteIdMap
    sid = lookupWorkerSiteId wid wcd
    swm = wcd_SiteToWorkerMap wcd
    sm = wcd_SiteMap wcd
    (swm', sm') = deleteSiteId sid
    deleteSiteId Nothing = (swm, sm)
    deleteSiteId (Just s) = (MMap.deleteElem s wid swm , deleteIdFromMap s sm)
    -- update the sitetonode and the siteIdMap
    as = lookupWorkerActions wid wcd
    awm = wcd_ActionToWorkerMap wcd
    awm' = deleteActions as
    deleteActions Nothing = awm
    deleteActions (Just s) = foldl (\m a -> MMap.deleteElem a wid m) awm s


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
  

deleteTaskFromWorker :: TaskId -> M.WorkerId -> WorkerControllerData -> WorkerControllerData
deleteTaskFromWorker tid wid wcd = wcd { wcd_TaskToWorkerMap = twm',  wcd_WorkerToTaskMap = wtm'}
  where
  twm' = MMap.deleteElem tid wid (wcd_TaskToWorkerMap wcd)
  wtm' = MMap.deleteElem wid tid (wcd_WorkerToTaskMap wcd)


deleteTaskFromWorkers :: TaskId -> WorkerControllerData -> WorkerControllerData
deleteTaskFromWorkers tid wcd = wcd''
  where
  wids = Set.toList $ MMap.lookup tid (wcd_TaskToWorkerMap wcd)
  wcd'' = foldl (\wcd' wid -> deleteTaskFromWorker tid wid wcd') wcd wids


sendStartTask :: WorkerController -> TaskData -> IO (TaskSendResult)
sendStartTask wc td
  = E.handle (\e -> 
              do 
              errorM localLogger $ "sendStartTask: " ++ show e
              return TSRError) $
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
            return (wcd, TSRNotSend)
          else do          
            infoM localLogger $ "starting Task: " ++ show (td_TaskId td)
            debugM localLogger $ "sendStartTask: wls:" ++ show wls
            let (_,wid) = head wls
            let wp = fromJust $ lookupWorkerPort wid wcd
            WC.startTask td wp
            let mm' = addTaskToWorker (td_TaskId td) wid wcd
            return (mm', TSRSend)




-- ----------------------------------------------------------------------------
-- typeclass instanciation
-- ----------------------------------------------------------------------------

instance Debug MasterData where

  printDebug md
    = do
      putStrLn "Master-Object (full)"
      withMVar (md_ServerThreadId md) $ 
        \i-> do putStrLn $ prettyRecordLine 15 "ServerId:" i
      putStrLn $ prettyRecordLine gap "OwnStream:" (md_OwnStream md)
      putStrLn $ prettyRecordLine gap "OwnPort:" (md_OwnPort md)
      withMVar (md_WorkerController md) $
        \wcd -> 
        do
        putStrLn $ prettyRecordLine gap "WorkerToSiteMap:" (wcd_WorkerToSiteMap wcd)
        putStrLn $ prettyRecordLine gap "SiteToWorkerMap:" (wcd_SiteToWorkerMap wcd)
        putStrLn $ prettyRecordLine gap "SiteMap:" (wcd_SiteMap wcd)
        putStrLn $ prettyRecordLine gap "Last NodeId:" (wcd_WorkerId wcd)
        putStrLn $ prettyRecordLine gap "JobMapMap:" (wcd_JobMap wcd)
        putStrLn $ "JobController:"
        jc <- printJobController (md_JobController md)
        putStrLn jc
      where
        gap = 20




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

  
  doMapReduce ji md
    = do
      debugM localLogger "doMapReduce"
      performJob ji (md_JobController md)




instance Master MasterData where


  closeMaster md 
    = do
      stopRequestDispatcher (md_ServerThreadId md)
      closeStream (md_OwnStream md)
      return ()

  getMasterRequestPort md = md_OwnPort md

  
  registerWorker sid po as md
    = do
      modifyMVar (md_WorkerController md) $
        \mm ->
        do
        -- create a new Id and a new Port
        let (wid, mm') = getNextWorkerId mm
        let wp = WP.newWorkerPort po
        -- add worker to master
        let mm'' = addWorkerToMaster wid sid wp as mm'
        return (mm'', (wid, md))        


  unregisterWorker workerId md
    = do
      modifyMVar (md_WorkerController md) $
        \mm ->
        do
        let mm' = deleteWorkerFromMaster workerId mm
        return (mm', md) 


  receiveTaskCompleted td md
    = do
      debugM localLogger $ "completed Task: " ++ show (td_TaskId td)
      setTaskCompleted (md_JobController md) td
      -- TODO inform other workers
      return md


  receiveTaskError td md
    = do
      debugM localLogger $ "error Task: " ++ show (td_TaskId td)
      setTaskError (md_JobController md) td
      -- TODO inform other workers
      return md