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
)
where

import           Prelude hiding (appendFile)

import           Control.Concurrent
import qualified Control.Exception as E
import           Data.Maybe
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import           System.Log.Logger

import           Holumbus.Common.Debug
import           Holumbus.Common.Utils
import qualified Holumbus.FileSystem.Controller as C
import qualified Holumbus.FileSystem.Node as N
import qualified Holumbus.FileSystem.Node.NodePort as NP
import qualified Holumbus.FileSystem.Messages as M
import qualified Holumbus.FileSystem.Storage as S

import           Holumbus.Network.Site
import           Holumbus.Network.Communication
import           Holumbus.Network.Messages

localLogger :: String
localLogger = "Holumbus.FileSystem.Controller"

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


-- type SiteData = (SiteId, NP.NodePort, Integer)

type FileToNodeMap = Map.Map S.FileId (Set.Set M.NodeId)
-- type NodeToSiteMap = Map.Map M.NodeId SiteData
-- type SiteToNodeMap = Map.Map SiteId (Set.Set M.NodeId)

data ControllerMaps = ControllerMaps {
    cm_FileToNodeMap :: ! FileToNodeMap
--  , cm_NodeToSiteMap :: ! NodeToSiteMap
--  , cm_SiteToNodeMap :: ! SiteToNodeMap
--  , cm_SiteMap       :: ! SiteMap
--  , cm_NodeId        :: ! M.NodeId
  }

data ControllerData = ControllerData {
    cd_Server :: Server
--    cd_ServerThreadId ::   MVar (Maybe ThreadId)
--  , cd_OwnStream      :: ! M.ControllerRequestStream
--  , cd_OwnPort        :: ! M.ControllerRequestPort
    , cd_Maps           ::   ControllerMaps
  }

data Controller = Controller (MVar ControllerData)


-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

{-
-- initialise the client
    node <- newEmptyMVar
    client <- newClient sn soid (dispatch (Node node))
    -- open storage
    stor' <- S.openStorage stor             
    putMVar node (NodeData client stor')
    return (Node node)
-}

newController :: StreamName -> Maybe PortNumber -> IO Controller
newController sn pn
  = do
    -- initialise the server
    controller <- newEmptyMVar
    let c = Controller controller
    server <- newServer sn pn (dispatch c) (Just $ registerNode c) (Just $ unregisterNode c)
    -- initialize values
    let maps = ControllerMaps Map.empty -- Map.empty Map.empty emptySiteMap 0 
    -- mapMVar <- newMVar maps
    -- st    <- (newGlobalStream sn::IO M.ControllerRequestStream)
    -- po    <- newPortFromStream st
    -- we can't start the server yet
    -- tid   <- newMVar Nothing
    -- get the internal data
    putMVar controller (ControllerData server maps)
    -- startRequestDispatcher tid st (dispatch cd)
    return (Controller controller)
  

dispatch 
  :: Controller 
  -> M.ControllerRequestMessage 
  -> IO (Maybe M.ControllerResponseMessage)
dispatch cd msg
  = do
    case msg of
      (M.CReqGetFileSites f) ->
        do
        s <- C.getFileSites f cd
        return $ Just $ M.CRspGetFileSites s
      (M.CReqContains f) ->
        do
        b <- C.containsFile f cd
        return $ Just $ M.CRspContains b
      (M.CReqGetNearestNodePortWithFile f sid) ->
        do
        p <- C.getNearestNodePortWithFile f sid cd
        return $ Just $ M.CRspGetNearestNodePortWithFile p
      (M.CReqGetNearestNodePortForFile f l sid) ->
        do
        p <- C.getNearestNodePortForFile f l sid cd
        return $ Just $ M.CRspGetNearestNodePortForFile p
      (M.CReqCreate f n) ->
        do
        C.createFile f n cd
        return $ Just $ M.CRspSuccess
      (M.CReqAppend f n) -> 
        do
        C.appendFile f n cd
        return $ Just $ M.CRspSuccess
      (M.CReqDelete f n) ->
        do
        C.deleteFile f n cd
        return $ Just $ M.CRspSuccess
      _ -> return Nothing


registerNode :: Controller -> IdType -> ClientPort -> IO ()
registerNode (Controller controller) i cp
  = do
    let np = NP.newNodePort cp
    fids <- N.getFileIds np
    modifyMVar controller $
      \cd ->
      do
      let cm = addFilesToController fids i (cd_Maps cd)
      return (cd {cd_Maps = cm}, ())


unregisterNode :: Controller -> IdType -> ClientPort -> IO ()
unregisterNode (Controller controller) i _
  = do
    modifyMVar controller $
      \cd ->
      do
      let cm = deleteFilesFromController i (cd_Maps cd)
      return (cd {cd_Maps = cm}, ())
    
    
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

{-
getNextId :: ControllerMaps -> (M.NodeId, ControllerMaps)
getNextId cm 
  = (nid, cm { cm_NodeId = nid })
  where
    nid = (cm_NodeId cm) + 1
-}

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

{-
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
-}



-- | gets the List of all sites the file is located on...
getFileClientInfoList :: S.FileId -> Server -> ControllerMaps -> IO [ClientInfo]
getFileClientInfoList f s (ControllerMaps fnm)
  = do
    let is = Set.toList $ maybe Set.empty id (Map.lookup f fnm)
    mbDats <- mapM (\i -> getClientInfo i s) is
    return $ catMaybes mbDats  



lookupNearestPortWithFile :: S.FileId -> SiteId -> Server -> ControllerMaps -> IO (Maybe ClientPort)
lookupNearestPortWithFile f sid s cm
  = do
    dats <- getFileClientInfoList f s cm
    let sids  = map (\ci -> ci_Site ci) dats
        mbns  = nearestId sid sids
        mbdat = maybe Nothing (\ns -> List.find (\ci -> (ci_Site ci) == ns) dats) mbns
        mbnp  = maybe Nothing (\ci -> Just $ ci_Port ci) mbdat 
    return mbnp


lookupNearestPortWithFileAndSpace :: S.FileId -> Integer -> SiteId -> Server -> ControllerMaps -> IO (Maybe ClientPort)
lookupNearestPortWithFileAndSpace f size sid s cm
  = lookupNearestPortWithFile f sid s cm
{-    dats <- getFileClientInfoList f s cm
    -- TODO add Capacity
    let sids  = map (\ci -> ci_Site ci) $ filter (...) dats
        mbns  = nearestId sid sids
        mbdat = maybe Nothing (\ns -> List.find (\(s,_,_) -> s == ns) dats) mbns
        mbnp  = maybe Nothing (\(_,np,_) -> Just np) mbdat
    return mbnp 
-}

lookupNearestPortWithSpace :: Integer -> SiteId -> Server -> ControllerMaps -> IO (Maybe ClientPort)
lookupNearestPortWithSpace size sid s cm
  = do
    dats <- getAllClientInfos s
    case dats of
      (ci:_) -> return $ Just $ ci_Port ci
      []     -> return Nothing 
{-    do
    dats <- getClientInfoList s cm
    -- TODO add capacity 
    let sids  = map (\ci -> ci_Site ci) $ filter (...) dats
        mbns  = nearestId sid sids
        mbdat = maybe Nothing (\ns -> List.find (\(s,_,_) -> s == ns) dats) mbns
        mbnp  = maybe Nothing (\(_,np,_) -> Just np) mbdat
    return mbnp
-}

lookupNearestPortForFile :: S.FileId -> Integer -> SiteId -> Server -> ControllerMaps -> IO (Maybe ClientPort)
lookupNearestPortForFile f size sid s cm
  -- if file exists, get nearest node, else the closest with space
  = do
    nodeWithFile    <- lookupNearestPortWithFileAndSpace f size sid s cm
    nodeWithoutFile <- lookupNearestPortWithSpace size sid s cm 
    let mbnp = maybe nodeWithoutFile (\np -> Just np) nodeWithFile
    return mbnp
    

getOtherFilePorts :: S.FileId -> IdType -> Server -> ControllerMaps -> IO [ClientInfo]
getOtherFilePorts f nid s (ControllerMaps fnm)
  = do
    -- get all nodes which hold the file without the given node
    let otherids = Set.toList $ Set.delete nid $ maybe Set.empty id (Map.lookup f fnm)
    mbDats <- mapM (\i -> getClientInfo i s) otherids    
    return $ catMaybes mbDats

  
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

instance C.ControllerClass Controller where

  closeController (Controller controller) 
    = modifyMVar controller $
        \cd ->
        do      
        closeServer (cd_Server cd)
        return (cd,())

  
  -- getFileSites :: S.FileId -> Controller -> IO (Set.Set SiteId) 
  getFileSites f (Controller controller)
    = withMVar (controller) $
        \cd ->
        do
        dats <- getFileClientInfoList f (cd_Server cd) (cd_Maps cd)
        let sids = map (\ci -> ci_Site ci) dats
        return (Set.fromList sids)

  
  -- containsFile :: S.FileId -> Controller -> IO Bool
  containsFile f (Controller controller)
    = withMVar (controller) $
        \cd -> return $ Map.member f (cm_FileToNodeMap $ cd_Maps cd)
  
      
  -- getNearestNodePortWithFile :: S.FileId -> SiteId -> c -> IO (Maybe M.NodeRequestPort)
  getNearestNodePortWithFile f sid (Controller controller)
    = withMVar (controller) $
        \cd -> lookupNearestPortWithFile f sid (cd_Server cd) (cd_Maps cd) 

 
  -- getNearestNodePortForFile :: S.FileId -> Integer -> SiteId -> c -> IO (Maybe M.NodeRequestPort)
  getNearestNodePortForFile f c sid (Controller controller)
    = withMVar (controller) $
        \cd -> lookupNearestPortForFile f c sid (cd_Server cd) (cd_Maps cd)


-- ----------------------------------------------------------------------------
-- used by the nodes
-- ----------------------------------------------------------------------------


--createFile :: S.FileId -> M.NodeId -> ControllerData -> IO ControllerData
  createFile f nid (Controller controller)
    = modifyMVar controller $
        \cd ->
        do
        let cm =  addFileToController f nid (cd_Maps cd)
        --TODO copy file
        return (cd {cd_Maps = cm}, ())


--appendFile :: S.FileId -> M.NodeId -> ControllerData -> IO ControllerData
--TODO
  appendFile f nid cd = C.createFile f nid cd


--deleteFile :: S.FileId -> M.NodeId -> ControllerData -> IO ControllerData
  deleteFile f nid (Controller controller)
    = modifyMVar controller $
        \cd ->
        do
        -- inform all other nodes to delete node
        cps <- getOtherFilePorts f nid (cd_Server cd) (cd_Maps cd)
        let nps = map (\ci -> NP.newNodePort (ci_Port ci)) cps
        deleteFileFromNodes f nps
        -- delete file from Controller
        let cm = deleteFileFromController f (cd_Maps cd)        
        return (cd {cd_Maps = cm}, ())
        
        

instance Debug Controller where
  printDebug (Controller controller)
    = do
      putStrLn "Controller-Object (full)"
      withMVar controller $
        \cd ->
        do
        putStrLn "--------------------------------------------------------"        
        putStrLn "Server"
        printDebug (cd_Server cd)
        putStrLn "--------------------------------------------------------"
        putStrLn "FileToNodeMap:"
        putStrLn $ show (cm_FileToNodeMap $ cd_Maps cd) 
        