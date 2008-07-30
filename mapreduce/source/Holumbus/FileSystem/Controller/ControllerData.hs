-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.FileSystem.Controller.ControllerData
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}

-- ----------------------------------------------------------------------------

module Holumbus.FileSystem.Controller.ControllerData
(
-- * datatypes
  ControllerData
  
-- * creation and destruction
, newController
, closeController
)
where

import Prelude hiding (appendFile)

import Control.Concurrent
import qualified Control.Exception as E

import Data.Maybe
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import System.Log.Logger

import Holumbus.Common.Utils
import qualified Holumbus.FileSystem.Controller as C
import qualified Holumbus.FileSystem.Node as N
import qualified Holumbus.FileSystem.Node.NodePort as NP
import qualified Holumbus.FileSystem.Messages as M
import qualified Holumbus.FileSystem.Storage as S

import qualified Holumbus.Network.Port as P
import Holumbus.Network.Site


localLogger :: String
localLogger = "Holumbus.FileSystem.Controller"

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


type SiteData = (SiteId, NP.NodePort, Integer)

type FileToNodeMap = Map.Map S.FileId (Set.Set M.NodeId)
type NodeToSiteMap = Map.Map M.NodeId SiteData
type SiteToNodeMap = Map.Map SiteId (Set.Set M.NodeId)

data ControllerMaps = ControllerMaps {
    cm_FileToNodeMap :: ! FileToNodeMap
  , cm_NodeToSiteMap :: ! NodeToSiteMap
  , cm_SiteToNodeMap :: ! SiteToNodeMap
  , cm_SiteMap       :: ! SiteMap
  , cm_NodeId        :: ! M.NodeId
  }

data ControllerData = ControllerData {
    cd_ServerThreadId ::   MVar (Maybe ThreadId)
  , cd_OwnStream      :: ! M.ControllerRequestStream
  , cd_OwnPort        :: ! M.ControllerRequestPort
  , cd_Maps           ::   MVar ControllerMaps
  }



-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


newController :: IO ControllerData
newController 
  = do
    -- initialize values
    let maps = ControllerMaps Map.empty Map.empty Map.empty emptySiteMap 0 
    mapMVar <- newMVar maps
    st    <- (P.newGlobalStream "filesystem"::IO M.ControllerRequestStream)
    po    <- ((P.newPortFromStream st)::IO M.ControllerRequestPort)
    -- we can't start the server yet
    tid   <- newMVar Nothing
    -- get the internal data
    cd <- startRequestDispatcher (ControllerData tid st po mapMVar)
    return cd


closeController :: ControllerData -> IO ()
closeController cd 
  = do
    -- shutdown the server thread and the stream
    cd' <- stopRequestDispatcher cd
    P.closeStream (cd_OwnStream cd')
    return ()      
  

startRequestDispatcher :: ControllerData -> IO ControllerData
startRequestDispatcher cd
  = do
    servId <- takeMVar (cd_ServerThreadId cd)
    servId' <- case servId of
      i@(Just _) -> return i
      (Nothing) ->
        do
        i <- forkIO $ requestDispatcher cd
        return (Just i)
    putMVar (cd_ServerThreadId cd) servId'
    return cd


stopRequestDispatcher :: ControllerData -> IO ControllerData
stopRequestDispatcher cd 
  = do
    servId <- takeMVar (cd_ServerThreadId cd)
    servId' <- case servId of
      (Nothing) -> return Nothing
      (Just i) -> 
        do
        E.throwDynTo i myThreadId
        yield
        return Nothing
    putMVar (cd_ServerThreadId cd) servId'
    return cd


requestDispatcher :: ControllerData -> IO ()
requestDispatcher cd
  = do
    E.handle (\e -> 
      do
      putStrLn $ show e
      yield
      requestDispatcher cd
     ) $
      do
      -- read the next message from the stream (block, if no message arrived)
      let stream = (cd_OwnStream cd)
      msg <- P.readStreamMsg stream
      -- extract the data
      let dat = P.getMessageData msg
      -- extract the (possible replyport)
      let replyPort = M.decodeControllerResponsePort $ P.getGenericData msg
      case replyPort of
        (Nothing) ->  
          do
          putStrLn "no reply port in message"
          yield
        (Just p) ->
          do
          -- putStrLn $ show p
          -- do the dispatching in a new process...
          _ <- forkIO $ dispatch cd dat p
          return ()
      --threadDelay 10
      requestDispatcher cd


dispatch 
  :: ControllerData 
  -> M.ControllerRequestMessage 
  -> M.ControllerResponsePort
  -> IO ()
dispatch cd msg replyPort
  = do
    case msg of
      (M.CReqRegister s p) ->
        do
        handleRequest replyPort (C.registerNode s p cd) (\(nid, _) -> M.CRspRegister nid)
        return ()
      (M.CReqUnregister n) ->
        do
        handleRequest replyPort (C.unregisterNode n cd) (\_ -> M.CRspUnregister)
        return ()
      (M.CReqGetFileSites f) ->
        do
        handleRequest replyPort (C.getFileSites f cd) (\s -> M.CRspGetFileSites s)
        return ()
      (M.CReqContains f) ->
        do
        handleRequest replyPort (C.containsFile f cd) (\b -> M.CRspContains b)
        return ()
      (M.CReqGetNearestNodePortWithFile f sid) ->
        do
        handleRequest replyPort (C.getNearestNodePortWithFile f sid cd) (\p -> M.CRspGetNearestNodePortWithFile p)
        return ()
      (M.CReqGetNearestNodePortForFile f l sid) ->
        do
        handleRequest replyPort (C.getNearestNodePortForFile f l sid cd) (\p -> M.CRspGetNearestNodePortForFile p)
        return ()
      (M.CReqCreate f n) ->
        do
        handleRequest replyPort (C.createFile f n cd) (\_ -> M.CRspSuccess)
        return ()
      (M.CReqAppend f n) -> 
        do
        handleRequest replyPort (C.appendFile f n cd) (\_ -> M.CRspSuccess)
        return ()
      (M.CReqDelete f n) ->
        do
        handleRequest replyPort (C.deleteFile f n cd) (\_ -> M.CRspSuccess)
        return ()
      _ -> handleRequest replyPort (return ()) (\_ -> M.CRspUnknown)


handleRequest
  :: M.ControllerResponsePort
  -> IO a
  -> (a -> M.ControllerResponseMessage) 
  -> IO ()
handleRequest po fhdl fres
  = do
    -- in case, we can't send the error...
    E.handle (\e -> errorM localLogger $ show e) $ do
      do
      -- in case our operation fails, we send a failure-response
      E.handle (\e -> P.send po (M.CRspError $ show e)) $
        do
        -- our action, might raise an exception
        r <- fhdl
        -- send the response
        P.send po $ fres r



-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


--newNodeData :: M.NodeId -> SiteId -> M.NodeRequestPort -> NodeData
--newNodeData nid sid po = NodeData nid sid po

{-

nodes2sites :: ControllerMaps -> [M.NodeId] ->  [SiteId]
nodes2sites (ControllerMaps _ nsm _ _ _) nids = lookupList nsm nids 
 
sites2ports :: ControllerMaps -> [SiteId] ->  [NP.NodePort]
sites2ports (ControllerMaps _ _ spm _ _) sids = lookupList spm sids 


-- | gets all nodessites
getNodesList :: ControllerMaps -> [M.NodeId]
getNodesList (ControllerMaps _ nsm _ _ _) = Map.keys nsm


-- TODO
getNodesWithSpace :: S.FileId -> Integer -> ControllerMaps -> [SiteId]
getNodesWithSpace _ _ cm = getSiteList cm


getOtherFileNodes :: S.FileId -> M.NodeId -> ControllerMaps -> [M.NodeId]
getOtherFileNodes f nid (ControllerMaps fnm _ _ _ _)
  = Set.toList $ Set.delete nid allNids
  where
    allNids = Map.findWithDefault (Set.empty) f fnm   



-- | the minimal occurrence of a file on the nodes
copyingLimit :: Int
copyingLimit = 2


-- | Get a list of all fileIds that should be copied.
--   That means that the file has not enough copies
-- TODO we only compare the nodeid... we should look for sites or hosts
getFilesForCopying :: FileToNodeMap -> [S.FileId]
getFilesForCopying fnm = (fst . unzip) filteredList 
  where
    filteredList = filter setSelector (Map.toList fnm)
    setSelector (_,s) = Set.size s < copyingLimit
-}

getNextId :: ControllerMaps -> (M.NodeId, ControllerMaps)
getNextId cm 
  = (nid, cm { cm_NodeId = nid })
  where
    nid = (cm_NodeId cm) + 1


-- | Adds the files of a node to the global directory.
addFilesToController :: [S.FileId] -> M.NodeId -> ControllerMaps -> ControllerMaps
addFilesToController fids nid cm 
  = cm { cm_FileToNodeMap = fnm' }
  where
    fnm = cm_FileToNodeMap cm
    fnm' = Map.unionWith combine fnm newMap
    newMap = Map.fromList $ zip fids (repeat $ Set.singleton nid)
    combine s1 s2 = Set.union s1 s2


-- | Deletes the files of a node from the global directory.
deleteFilesFromController :: M.NodeId -> ControllerMaps -> ControllerMaps
deleteFilesFromController nid cm
  = cm { cm_FileToNodeMap = fnm' }
  where 
    fnm = cm_FileToNodeMap cm 
    fnm' = Map.fromList filteredList
    filteredList = filter (\(_,s) -> s /= Set.empty) list
    list = map (\(k,s) -> (k, Set.delete nid s)) (Map.toList fnm)


addFileToController :: S.FileId -> M.NodeId -> ControllerMaps -> ControllerMaps
addFileToController fid nid cm = addFilesToController [fid] nid cm


deleteFileFromController :: S.FileId -> ControllerMaps -> ControllerMaps
deleteFileFromController fid cm
  = cm { cm_FileToNodeMap = fnm' }
  where 
    fnm = cm_FileToNodeMap cm 
    fnm' = Map.delete fid fnm


lookupNodePort :: M.NodeId -> ControllerMaps -> Maybe NP.NodePort
lookupNodePort nid cm = getPort sd
  where
    nsm = cm_NodeToSiteMap cm
    sd = Map.lookup nid nsm
    getPort Nothing = Nothing
    getPort (Just (_, np, _)) = Just np
    
    
lookupNodeSiteId :: M.NodeId -> ControllerMaps -> Maybe SiteId
lookupNodeSiteId nid cm = convertSite sd
  where
    nsm = cm_NodeToSiteMap cm
    sd = Map.lookup nid nsm
    convertSite Nothing = Nothing
    convertSite (Just (sid, _, _)) = Just sid

    
addNodeToController :: M.NodeId -> SiteId -> NP.NodePort -> ControllerMaps -> ControllerMaps
addNodeToController nid sid np cm
  = cm { cm_NodeToSiteMap = nsm', cm_SiteToNodeMap = snm', cm_SiteMap = sm' }
  where
    --update the nodetosite map
    nsm = cm_NodeToSiteMap cm
    nsm' = Map.insert nid (sid, np, 100000000) nsm
    --update the sitetonode map
    snm = cm_SiteToNodeMap cm
    snm' = Map.alter altering sid snm
    altering Nothing = Just $ Set.singleton nid
    altering (Just s) = Just $ Set.insert nid s
    -- update the SiteMap
    sm = cm_SiteMap cm
    sm' = addIdToMap sid sm
        

deleteNodeFromController :: M.NodeId -> ControllerMaps -> ControllerMaps
deleteNodeFromController nid cm 
  = cm { cm_NodeToSiteMap = nsm', cm_SiteToNodeMap = snm', cm_SiteMap = sm' }
  where
    --update the nodetosite map
    nsm = cm_NodeToSiteMap cm
    nsm' = Map.delete nid nsm
    --update the sitetonode and the siteIdMap
    sid = lookupNodeSiteId nid cm
    snm = cm_SiteToNodeMap cm
    sm = cm_SiteMap cm
    (snm', sm') = deleteSiteId sid
    deleteSiteId Nothing = (snm, sm)
    deleteSiteId (Just s) = (Map.alter delSet s snm , deleteIdFromMap s sm)
      where
        delSet Nothing = Nothing
        delSet (Just set) = filterEmpty $ Set.delete nid set
        filterEmpty set
          | set == Set.empty = Nothing
          | otherwise = Just set


getSiteDataList :: ControllerMaps -> [SiteData]
getSiteDataList (ControllerMaps _ nsm _ _ _) = Map.elems nsm


-- | gets the List of all sites the file is located on...
getFileSiteDataList :: S.FileId -> ControllerMaps -> [SiteData]
getFileSiteDataList f (ControllerMaps fnm nsm _ _ _) = dats
  where
    nids = Set.toList $ maybe Set.empty id (Map.lookup f fnm)
    dats = mapMaybe (\nid -> Map.lookup nid nsm) nids 


lookupNearestPortWithFile :: S.FileId -> SiteId -> ControllerMaps -> Maybe NP.NodePort
lookupNearestPortWithFile f sid cm
  = maybe Nothing (\(_,np,_) -> Just np) mbdat
  where
    dats = getFileSiteDataList f cm
    sids = map (\(s,_,_) -> s) dats
    mbns = nearestId sid sids
    mbdat = maybe Nothing (\ns -> List.find (\(s,_,_) -> s == ns) dats) mbns


lookupNearestPortWithFileAndSpace :: S.FileId -> Integer -> SiteId -> ControllerMaps -> Maybe NP.NodePort
lookupNearestPortWithFileAndSpace f size sid cm
  = maybe Nothing (\(_,np,_) -> Just np) mbdat
  where
    dats = getFileSiteDataList f cm
    sids = map (\(s,_,_) -> s) $ filter (\(_,_,c) -> c >= size) dats
    mbns = nearestId sid sids
    mbdat = maybe Nothing (\ns -> List.find (\(s,_,_) -> s == ns) dats) mbns
 

lookupNearestPortWithSpace :: Integer -> SiteId -> ControllerMaps -> Maybe NP.NodePort
lookupNearestPortWithSpace size sid cm
  = maybe Nothing (\(_,np,_) -> Just np) mbdat
  where
    dats = getSiteDataList cm
    sids = map (\(s,_,_) -> s) $ filter (\(_,_,c) -> c >= size) dats
    mbns = nearestId sid sids
    mbdat = maybe Nothing (\ns -> List.find (\(s,_,_) -> s == ns) dats) mbns


lookupNearestPortForFile :: S.FileId -> Integer -> SiteId -> ControllerMaps -> Maybe NP.NodePort
lookupNearestPortForFile f size sid cm
  -- if file exists, get nearest node, else the closest with space
  = maybe nodeWithoutFile (\np -> Just np) nodeWithFile
  where 
    nodeWithFile =  lookupNearestPortWithFileAndSpace f size sid cm
    nodeWithoutFile = lookupNearestPortWithSpace size sid cm 


getOtherFileNodePorts :: S.FileId -> M.NodeId -> ControllerMaps -> [NP.NodePort]
getOtherFileNodePorts f nid cm@(ControllerMaps fnm _ _ _ _) = ps
  where
    othernids = Set.toList $ Set.delete nid $ maybe Set.empty id (Map.lookup f fnm)
    ps = mapMaybe (\i -> lookupNodePort i cm) othernids
  
  
deleteFileFromNodes :: S.FileId -> [NP.NodePort] -> IO ()
deleteFileFromNodes fid nps = sequence_ $ map deleteFileFromNode nps
  where
    deleteFileFromNode np 
      = do
        E.handle (\e -> putStrLn $ show e) $
          do 
          -- send a delete-request to the node 
          -- but don't inform the controller again (False)
          N.deleteFile fid False np
          return ()
          
-- ----------------------------------------------------------------------------
--
-- ---------------------------------------------------------------------------- 

instance C.Controller ControllerData where

  getFileIds nid cd
    = do
      modifyMVar (cd_Maps cd) $
        \cm ->
        do
        let np = lookupNodePort nid cm
        case np of 
          (Nothing) -> return (cm, ())
          (Just np') ->
            do
            --insert all Files into fsm
            fids <- N.getFileIds np'
            let cm' = addFilesToController fids nid cm
            return (cm', ())        

        
  getControllerRequestPort cd = (cd_OwnPort cd)
  

--  registerNode :: SiteId -> M.NodeRequestPort -> ControllerData -> IO (M.NodeId, ControllerData)
  registerNode sid po cd
    = do
      modifyMVar (cd_Maps cd) $
        \cm ->
        do
        -- create a new Id and a new Port
        let (nid, cm') = getNextId cm
        let np = NP.newNodePort po
        -- add node to controller
        let cm'' = addNodeToController nid sid np cm'
        --insert all Files into fsm
        fids <- N.getFileIds np
        let cm''' = addFilesToController fids nid cm''
        return (cm''', (nid, cd))        


--unregisterNode :: M.NodeId -> ControllerData -> IO ControllerData
  unregisterNode nodeId cd
    = do
      modifyMVar (cd_Maps cd) $
        \cm ->
        do
        let cm' = deleteFilesFromController nodeId cm
        let cm'' = deleteNodeFromController nodeId cm'
        return (cm'', cd) 
        
  
--  getFileSites :: S.FileId -> ControllerData -> IO (Set.Set SiteId) 
  getFileSites f cd
    = do
      withMVar (cd_Maps cd) $
        \cm ->
        do
        let dats = getFileSiteDataList f cm
        let sids = map (\(s,_,_) -> s) dats
        return (Set.fromList sids)

  
--containsFile :: S.FileId -> ControllerData -> IO Bool
  containsFile f cd
    = do
      withMVar (cd_Maps cd) $
        \(ControllerMaps fsm _ _ _ _) -> return $ Map.member f fsm
  
      
--  getNearestNodePortWithFile :: S.FileId -> SiteId -> c -> IO (Maybe M.NodeRequestPort)
  getNearestNodePortWithFile f sid cd
    = do
      withMVar (cd_Maps cd) $
        \cm ->
        do
        let np = lookupNearestPortWithFile f sid cm 
        let po = maybe Nothing (\np' -> Just $ N.getNodeRequestPort np') np
        return po        

 
--  getNearestNodePortForFile :: S.FileId -> Integer -> SiteId -> c -> IO (Maybe M.NodeRequestPort)
  getNearestNodePortForFile f c sid cd
    = do
      withMVar (cd_Maps cd) $
        \cm ->
        do
        let np = lookupNearestPortForFile f c sid cm
        let po = maybe Nothing (\np' -> Just $ N.getNodeRequestPort np') np
        return po 
      


-- ----------------------------------------------------------------------------
-- used by the nodes
-- ----------------------------------------------------------------------------


{-
getNearestNeighbor :: SiteId -> ControllerData -> IO (Set.Set SiteId)
getNearestNeighbor sid cd
  = do
    handle (\e -> return (Left $ show e)) $
      do
      s <- withMVar (cd_maps cd) $
        \(ControllerMaps _ _ _ sm _) ->
        do
        let sids = getNeighbourSiteIds sid sm
        return sids
      return (Right s)
-}


--createFile :: S.FileId -> M.NodeId -> ControllerData -> IO ControllerData
  createFile f nid cd
    = do
      modifyMVar (cd_Maps cd) $
        \cm ->
        do
        let cm' = addFileToController f nid cm
        --TODO copy file
        return (cm', cd)


--appendFile :: S.FileId -> M.NodeId -> ControllerData -> IO ControllerData
--TODO
  appendFile f nid cd = C.createFile f nid cd


--deleteFile :: S.FileId -> M.NodeId -> ControllerData -> IO ControllerData
  deleteFile f nid cd
    = do
      modifyMVar (cd_Maps cd) $
        \cm ->
        do
        -- inform all other nodes to delete node
        let nps = getOtherFileNodePorts f nid cm
        deleteFileFromNodes f nps
        -- delete file from Controller
        let cm' = deleteFileFromController f cm        
        return (cm', cd)


  printDebug cd
    = do
      putStrLn "Controller-Object (full)"
      withMVar (cd_ServerThreadId cd) $ 
        \i-> do putStrLn $ prettyRecordLine 15 "ServerId:" i
      putStrLn $ prettyRecordLine gap "OwnStream:" (cd_OwnStream cd)
      putStrLn $ prettyRecordLine gap "OwnPort:" (cd_OwnPort cd)
      withMVar (cd_Maps cd) $
        \cm -> 
        do
        putStrLn $ prettyRecordLine gap "FileToNodeMap:" (cm_FileToNodeMap cm)
        putStrLn $ prettyRecordLine gap "NoteToSiteMap:" (cm_NodeToSiteMap cm)
        putStrLn $ prettyRecordLine gap "SiteToNodeMap:" (cm_SiteToNodeMap cm)
        putStrLn $ prettyRecordLine gap "SiteMap:"       (cm_SiteMap cm)
        putStrLn $ prettyRecordLine gap "Last NodeId:"   (cm_NodeId cm)
      where
        gap = 20

        