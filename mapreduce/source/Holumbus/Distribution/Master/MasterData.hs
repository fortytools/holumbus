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
, closeMaster
)
where


import           Control.Concurrent
import qualified Control.Exception as E

import           Data.List
import           Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import           System.Log.Logger

import           Holumbus.Network.Site
import qualified Holumbus.Network.Port as P
import           Holumbus.MapReduce.JobController
import           Holumbus.MapReduce.Types
import qualified Holumbus.Data.MultiMap as MMap
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


type SiteData = (SiteId, WP.WorkerPort)

type WorkerToSiteMap = Map.Map M.WorkerId SiteData
type SiteToWorkerMap = Map.Map SiteId (Set.Set M.WorkerId)

type TaskToWorkerMap = MMap.MultiMap TaskId M.WorkerId
type WorkerToTaskMap = MMap.MultiMap M.WorkerId TaskId

type JobMap = Map.Map JobId JobData

data WorkerControllerData = WorkerControllerData {
    wcd_WorkerToSiteMap :: ! WorkerToSiteMap
  , wcd_SiteToWorkerMap :: ! SiteToWorkerMap
  , wcd_SiteMap         :: ! SiteMap
  , wcd_WorkerId        :: ! M.WorkerId
  , wcd_JobMap          :: ! JobMap
  , wcd_TaskToWorkerMap :: ! TaskToWorkerMap
  , wcd_WorkerToTaskMap :: ! WorkerToTaskMap
  }

type WorkerController = MVar WorkerControllerData

data MasterData = MasterData {
    md_ServerThreadId   ::   MVar (Maybe ThreadId)
  , md_OwnStream        :: ! M.MasterRequestStream
  , md_OwnPort          :: ! M.MasterRequestPort
  , md_WorkerController ::   WorkerController
  , md_JobController    ::   JobController
  }



-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

newWorkerController :: IO WorkerController
newWorkerController
  = do
    let wcd = WorkerControllerData
                Map.empty
                Map.empty
                emptySiteMap
                0
                Map.empty
                MMap.empty
                MMap.empty
    wc <- newMVar wcd
    return wc

newMaster :: MapActionMap -> ReduceActionMap -> IO MasterData
newMaster mm rm
  = do
    -- initialize values 
    st    <- (P.newStream::IO M.MasterRequestStream)
    po    <- ((P.newPort st)::IO M.MasterRequestPort)

    -- we can't start the server yet
    tid   <- newMVar Nothing

    -- start the WorkerController
    wc <- newWorkerController

    -- start the JobController
    jc <- newJobController
    -- configure the JobController
    setTaskSendHook (sendStartTask wc) jc
    setMapActions mm jc
    setReduceActions rm jc

    -- get the internal data
    md <- startRequestDispatcher (MasterData tid st po wc jc)
    return md


closeMaster :: MasterData -> IO ()
closeMaster md 
  = do
    -- shutdown the server thread and the stream
    md' <- stopRequestDispatcher md
    P.closeStream (md_OwnStream md')
    return ()      
  

startRequestDispatcher :: MasterData -> IO MasterData
startRequestDispatcher md
  = do
    modifyMVar (md_ServerThreadId md) $
      \servId ->
      do        
      servId' <- case servId of
        i@(Just _) -> return i
        (Nothing) ->
          do
          i <- forkIO $ requestDispatcher md
          return (Just i)
      return (servId', md)


stopRequestDispatcher :: MasterData -> IO MasterData
stopRequestDispatcher md
  = do
    modifyMVar (md_ServerThreadId md) $
      \servId ->
      do
      servId' <- case servId of
        (Nothing) -> return Nothing
        (Just i) -> 
          do
          E.throwDynTo i myThreadId
          yield
          return Nothing
      return (servId', md)


requestDispatcher :: MasterData -> IO ()
requestDispatcher md
  = do
    E.handle (\e -> 
      do
      errorM localLogger $ "requestDispatcher: " ++ show e
      yield
      requestDispatcher md
     ) $
      do
      -- read the next message from the stream (block, if no message arrived)
      let stream = (md_OwnStream md)
      msg <- P.readStreamMsg stream
      -- extract the data
      let dat = P.getMessageData msg
      -- extract the (possible replyport)
      let replyPort = M.decodeMasterResponsePort $ P.getGenericData msg
      case replyPort of
        (Nothing) ->  
          do
          errorM localLogger $ "requestDispatcher: no reply port in message"
          yield
        (Just p) ->
          do
          -- putStrLn $ show p
          -- do the dispatching in a new process...
          _ <- forkIO $ dispatch md dat p
          return ()
      --threadDelay 10
      requestDispatcher md


dispatch 
  :: MasterData 
  -> M.MasterRequestMessage 
  -> M.MasterResponsePort
  -> IO ()
dispatch md msg replyPort
  = do
    case msg of
      (M.MReqRegister s p) ->
        do
        handleRequest replyPort (MC.registerWorker s p md) (\(nid, _) -> M.MRspRegister nid)
        return ()
      (M.MReqUnregister n) ->
        do
        handleRequest replyPort (MC.unregisterWorker n md) (\_ -> M.MRspUnregister)
        return ()
      (M.MReqAddJob ji) ->
        do
        handleRequest replyPort (MC.addJob ji md) (\_ -> M.MRspSuccess)
        return ()
      (M.MReqSingleStep) ->
        do
        handleRequest replyPort (MC.doSingleStep md) (\_ -> M.MRspSuccess)
        return ()
      (M.MReqTaskCompleted td) ->
        do
        handleRequest replyPort (MC.receiveTaskCompleted td md) (\_ -> M.MRspSuccess)
        return ()
      (M.MReqTaskError td) ->
        do
        handleRequest replyPort (MC.receiveTaskError td md) (\_ -> M.MRspSuccess)
        return ()
      _ -> handleRequest replyPort (return ()) (\_ -> M.MRspUnknown)

handleRequest
  :: M.MasterResponsePort
  -> IO a
  -> (a -> M.MasterResponseMessage) 
  -> IO ()
handleRequest po fhdl fres
  = do
    -- in case, we can't send the error...
    E.handle (\e -> errorM localLogger $ show e) $ do
      do
      -- in case our operation fails, we send a failure-response
      E.handle (\e -> P.send po (M.MRspError $ show e)) $
        do
        -- our action, might raise an exception
        r <- fhdl
        -- send the response
        P.send po $ fres r



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
    wsm = wcd_WorkerToSiteMap wcd
    sd = Map.lookup wid wsm
    convertSite Nothing = Nothing
    convertSite (Just (sid, _)) = Just sid


lookupWorkerPort :: M.WorkerId -> WorkerControllerData -> Maybe WP.WorkerPort
lookupWorkerPort wid wcd = getPort sd
  where
    wsm = wcd_WorkerToSiteMap wcd
    sd = Map.lookup wid wsm
    getPort Nothing = Nothing
    getPort (Just (_, wp)) = Just wp


addWorkerToMaster :: M.WorkerId -> SiteId -> WP.WorkerPort -> WorkerControllerData -> WorkerControllerData
addWorkerToMaster wid sid wp wcd
  = wcd { wcd_WorkerToSiteMap = wsm', wcd_SiteToWorkerMap = swm', wcd_SiteMap = sm' }
  where
    --update the nodetosite map
    wsm = wcd_WorkerToSiteMap wcd
    wsm' = Map.insert wid (sid, wp) wsm
    --update the sitetonode map
    swm = wcd_SiteToWorkerMap wcd
    swm' = Map.alter altering sid swm
    altering Nothing = Just $ Set.singleton wid
    altering (Just s) = Just $ Set.insert wid s
    -- update the SiteMap
    sm = wcd_SiteMap wcd
    sm' = addIdToMap sid sm
        

deleteWorkerFromMaster :: M.WorkerId -> WorkerControllerData -> WorkerControllerData
deleteWorkerFromMaster wid wcd
  = wcd { wcd_WorkerToSiteMap = wsm', wcd_SiteToWorkerMap = swm', wcd_SiteMap = sm' }
  where
    --update the nodetosite map
    wsm = wcd_WorkerToSiteMap wcd
    wsm' = Map.delete wid wsm
    --update the sitetonode and the siteIdMap
    sid = lookupWorkerSiteId wid wcd
    swm = wcd_SiteToWorkerMap wcd
    sm = wcd_SiteMap wcd
    (swm', sm') = deleteSiteId sid
    deleteSiteId Nothing = (swm, sm)
    deleteSiteId (Just s) = (Map.alter delSet s swm , deleteIdFromMap s sm)
      where
        delSet Nothing = Nothing
        delSet (Just set) = filterEmpty $ Set.delete wid set
        filterEmpty set
          | set == Set.empty = Nothing
          | otherwise = Just set


getTasksPerWorker :: WorkerControllerData -> [(Int, M.WorkerId)]
getTasksPerWorker wcd = nullList ++ sortedList
  where
  wtm = wcd_WorkerToTaskMap wcd
  -- all WorkerIds
  allWids  = Set.fromList $ Map.keys $ wcd_WorkerToSiteMap wcd
  -- all WorkerIds with Tasks
  taskWids = MMap.keys wtm
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
        let wls = getTasksPerWorker wcd
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
-- typeclass instanciation
-- ----------------------------------------------------------------------------


instance Master MasterData where

  getMasterRequestPort md = md_OwnPort md
  
  registerWorker sid po md
    = do
      modifyMVar (md_WorkerController md) $
        \mm ->
        do
        -- create a new Id and a new Port
        let (wid, mm') = getNextWorkerId mm
        let wp = WP.newWorkerPort po
        -- add worker to master
        let mm'' = addWorkerToMaster wid sid wp mm'
        return (mm'', (wid, md))        


  unregisterWorker workerId md
    = do
      modifyMVar (md_WorkerController md) $
        \mm ->
        do
        let mm' = deleteWorkerFromMaster workerId mm
        return (mm', md) 


  addJob ji md
    = do
      r <- startJob ji (md_JobController md)
      case r of
        (Left m) -> putStrLn m
        (Right (_,res)) -> printJobResult res
      return md
      

  doSingleStep md
    = do
      debugM localLogger "doSingleStep"
      singleStepJobControlling (md_JobController md)
      return md


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
        
        
        
