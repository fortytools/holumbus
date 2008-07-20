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

import qualified Data.Map as Map
import qualified Data.Set as Set

import           System.Log.Logger

import           Holumbus.Network.Site
import qualified Holumbus.Network.Port as P
import           Holumbus.MapReduce.JobController
import           Holumbus.MapReduce.Types
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

type JobMap = Map.Map JobId JobData

data MasterMaps = MasterMaps {
    mm_WorkerToSiteMap :: ! WorkerToSiteMap
  , mm_SiteToWorkerMap :: ! SiteToWorkerMap
  , mm_SiteMap         :: ! SiteMap
  , mm_WorkerId        :: ! M.WorkerId
  , mm_JobMap          :: ! JobMap
  }

data MasterData = MasterData {
    md_ServerThreadId ::   MVar (Maybe ThreadId)
  , md_OwnStream      :: ! M.MasterRequestStream
  , md_OwnPort        :: ! M.MasterRequestPort
  , md_Maps           ::   MVar MasterMaps
  , md_JobController  ::   JobController
  }



-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


newMaster :: MapActionMap -> ReduceActionMap -> IO MasterData
newMaster mm rm
  = do
    -- initialize values
    let maps = MasterMaps Map.empty Map.empty emptySiteMap 0 Map.empty 
    mapMVar <- newMVar maps
    st    <- (P.newStream::IO M.MasterRequestStream)
    po    <- ((P.newPort st)::IO M.MasterRequestPort)

    -- we can't start the server yet
    tid   <- newMVar Nothing

    -- start the jobController
    jc <- newJobController
    -- configure the JobController
    setTaskSendHook (sendStartTask) jc
    setMapActions mm jc
    setReduceActions rm jc

    -- get the internal data
    md <- startRequestDispatcher (MasterData tid st po mapMVar jc)
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
      putStrLn $ show e
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
          putStrLn "no reply port in message"
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


getNextWorkerId :: MasterMaps -> (M.WorkerId, MasterMaps)
getNextWorkerId mm 
  = (wid, mm { mm_WorkerId = wid })
  where
    wid = (mm_WorkerId mm) + 1


lookupWorkerSiteId :: M.WorkerId -> MasterMaps -> Maybe SiteId
lookupWorkerSiteId wid mm = convertSite sd
  where
    wsm = mm_WorkerToSiteMap mm
    sd = Map.lookup wid wsm
    convertSite Nothing = Nothing
    convertSite (Just (sid, _)) = Just sid


lookupWorkerPort :: M.WorkerId -> MasterMaps -> Maybe WP.WorkerPort
lookupWorkerPort wid mm = getPort sd
  where
    wsm = mm_WorkerToSiteMap mm
    sd = Map.lookup wid wsm
    getPort Nothing = Nothing
    getPort (Just (_, wp)) = Just wp


addWorkerToMaster :: M.WorkerId -> SiteId -> WP.WorkerPort -> MasterMaps -> MasterMaps
addWorkerToMaster wid sid wp mm
  = mm { mm_WorkerToSiteMap = wsm', mm_SiteToWorkerMap = swm', mm_SiteMap = sm' }
  where
    --update the nodetosite map
    wsm = mm_WorkerToSiteMap mm
    wsm' = Map.insert wid (sid, wp) wsm
    --update the sitetonode map
    swm = mm_SiteToWorkerMap mm
    swm' = Map.alter altering sid swm
    altering Nothing = Just $ Set.singleton wid
    altering (Just s) = Just $ Set.insert wid s
    -- update the SiteMap
    sm = mm_SiteMap mm
    sm' = addIdToMap sid sm
        

deleteWorkerFromMaster :: M.WorkerId -> MasterMaps -> MasterMaps
deleteWorkerFromMaster wid mm 
  = mm { mm_WorkerToSiteMap = wsm', mm_SiteToWorkerMap = swm', mm_SiteMap = sm' }
  where
    --update the nodetosite map
    wsm = mm_WorkerToSiteMap mm
    wsm' = Map.delete wid wsm
    --update the sitetonode and the siteIdMap
    sid = lookupWorkerSiteId wid mm
    swm = mm_SiteToWorkerMap mm
    sm = mm_SiteMap mm
    (swm', sm') = deleteSiteId sid
    deleteSiteId Nothing = (swm, sm)
    deleteSiteId (Just s) = (Map.alter delSet s swm , deleteIdFromMap s sm)
      where
        delSet Nothing = Nothing
        delSet (Just set) = filterEmpty $ Set.delete wid set
        filterEmpty set
          | set == Set.empty = Nothing
          | otherwise = Just set


sendStartTask :: TaskData -> IO (TaskSendResult)
sendStartTask td
  = do
    putStrLn "starting Task"
    putStrLn "TaskData:"
    putStrLn $ show td
    return TSRSend


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
      decodeResult ls = decodeTupleList ls


-- ----------------------------------------------------------------------------
-- typeclass instanciation
-- ----------------------------------------------------------------------------


instance Master MasterData where

  getMasterRequestPort md = md_OwnPort md
  
  registerWorker sid po md
    = do
      modifyMVar (md_Maps md) $
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
      modifyMVar (md_Maps md) $
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
      putStrLn "doSingleStep"
      singleStepJobControlling (md_JobController md)
      return md


  receiveTaskCompleted td md
    = do
      putStrLn "Task completed"
      putStrLn "TaskData:"
      putStrLn $ show td
      setTaskCompleted (md_JobController md) td
      -- TODO inform other workers
      return md


  receiveTaskError td md
    = do
      putStrLn "Task error"
      putStrLn "TaskData:"
      putStrLn $ show td
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
      withMVar (md_Maps md) $
        \mm -> 
        do
        putStrLn $ prettyRecordLine gap "WorkerToSiteMap:" (mm_WorkerToSiteMap mm)
        putStrLn $ prettyRecordLine gap "SiteToWorkerMap:" (mm_SiteToWorkerMap mm)
        putStrLn $ prettyRecordLine gap "SiteMap:" (mm_SiteMap mm)
        putStrLn $ prettyRecordLine gap "Last NodeId:" (mm_WorkerId mm)
        putStrLn $ prettyRecordLine gap "JobMapMap:" (mm_JobMap mm)
      where
        gap = 20
        
        
        
