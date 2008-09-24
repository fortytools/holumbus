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
import qualified Holumbus.FileSystem.Controller as C
import qualified Holumbus.FileSystem.Node as N
import qualified Holumbus.FileSystem.Node.NodePort as NP
import qualified Holumbus.FileSystem.Messages as M
import qualified Holumbus.FileSystem.Storage as S
import           Holumbus.Network.Site
import           Holumbus.Network.Communication


localLogger :: String
localLogger = "Holumbus.FileSystem.Controller"

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

type FileToNodeMap = Map.Map S.FileId (Set.Set M.NodeId)

data FileControllerData = FileControllerData {
    cm_FileToNodeMap :: ! FileToNodeMap
  }

type FileController = MVar FileControllerData


data ControllerData = ControllerData {
    cd_Server         :: Server
  , cd_FileController :: FileController
  }


-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------
newFileController :: IO FileController
newFileController
  = do
    let fc = FileControllerData Map.empty
    newMVar fc


newController :: StreamName -> Maybe PortNumber -> IO ControllerData
newController sn pn
  = do
    -- initialise the server
    c <- newEmptyMVar
    server <- newServer sn pn (dispatch c) (Just $ registerNode c) (Just $ unregisterNode c)
    -- initialize values
    fc <- newFileController
    let con = ControllerData server fc
    putMVar c con    
    return con
  

dispatch 
  :: MVar ControllerData 
  -> M.ControllerRequestMessage 
  -> IO (Maybe M.ControllerResponseMessage)
dispatch c msg
  = do
    cd <-readMVar c
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


registerNode :: MVar ControllerData -> IdType -> ClientPort -> IO ()
registerNode c i cp
  = do
    let np = NP.newNodePort cp
    fids <- N.getFileIds np
    cd <- readMVar c
    modifyMVar (cd_FileController cd) $
      \fc ->
      do
      let fc' = addFilesToController fids i fc
      return (fc', ())


unregisterNode :: MVar ControllerData -> IdType -> ClientPort -> IO ()
unregisterNode c i _
  = do
    debugM localLogger "unregisterNode: start"
    cd <- readMVar c
    modifyMVar (cd_FileController cd) $
      \fc ->
      do
      let fc' = deleteFilesFromController i fc
      return (fc', ())
    debugM localLogger "unregisterNode: end"
    
    
-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


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


-- | Adds the files of a node to the global directory.
addFilesToController :: [S.FileId] -> M.NodeId -> FileControllerData -> FileControllerData
addFilesToController fids nid cm 
  = cm { cm_FileToNodeMap = fnm' }
  where
    fnm = cm_FileToNodeMap cm
    fnm' = Map.unionWith combine fnm newMap
    newMap = Map.fromList $ zip fids (repeat $ Set.singleton nid)
    combine s1 s2 = Set.union s1 s2


-- | Deletes the files of a node from the global directory.
deleteFilesFromController :: M.NodeId -> FileControllerData -> FileControllerData
deleteFilesFromController nid cm
  = cm { cm_FileToNodeMap = fnm' }
  where 
    fnm = cm_FileToNodeMap cm 
    fnm' = Map.fromList filteredList
    filteredList = filter (\(_,s) -> s /= Set.empty) list
    list = map (\(k,s) -> (k, Set.delete nid s)) (Map.toList fnm)


addFileToController :: S.FileId -> M.NodeId -> FileControllerData -> FileControllerData
addFileToController fid nid cm = addFilesToController [fid] nid cm


deleteFileFromController :: S.FileId -> FileControllerData -> FileControllerData
deleteFileFromController fid cm
  = cm { cm_FileToNodeMap = fnm' }
  where 
    fnm = cm_FileToNodeMap cm 
    fnm' = Map.delete fid fnm
    

-- | gets the List of all sites the file is located on...
getFileClientInfoList :: S.FileId -> Server -> FileControllerData -> IO [ClientInfo]
getFileClientInfoList f s cm
  = do
    let fnm = cm_FileToNodeMap cm
    let is = Set.toList $ maybe Set.empty id (Map.lookup f fnm)
    mbDats <- mapM (\i -> getClientInfo i s) is
    return $ catMaybes mbDats  



lookupNearestPortWithFile :: S.FileId -> SiteId -> Server -> FileControllerData -> IO (Maybe ClientPort)
lookupNearestPortWithFile f sid s cm
  = do
    dats <- getFileClientInfoList f s cm
    let sids  = map (\ci -> ci_Site ci) dats
        mbns  = nearestId sid sids
        mbdat = maybe Nothing (\ns -> List.find (\ci -> (ci_Site ci) == ns) dats) mbns
        mbnp  = maybe Nothing (\ci -> Just $ ci_Port ci) mbdat 
    return mbnp


lookupNearestPortWithFileAndSpace :: S.FileId -> Integer -> SiteId -> Server -> FileControllerData -> IO (Maybe ClientPort)
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

-- TODO add capacity
lookupNearestPortWithSpace :: Integer -> SiteId -> Server -> FileControllerData -> IO (Maybe ClientPort)
lookupNearestPortWithSpace size sid s cm
  = do
    dats <- getAllClientInfos s 
    let sids  = map (\ci -> ci_Site ci) dats
        mbns  = nearestId sid sids
        mbdat = maybe Nothing (\ns -> List.find (\ci -> (ci_Site ci) == ns) dats) mbns
        mbnp  = maybe Nothing (\ci -> Just $ ci_Port ci) mbdat 
    return mbnp


lookupNearestPortForFile :: S.FileId -> Integer -> SiteId -> Server -> FileControllerData -> IO (Maybe ClientPort)
lookupNearestPortForFile f size sid s cm
  -- if file exists, get nearest node, else the closest with space
  = do
    nodeWithFile    <- lookupNearestPortWithFileAndSpace f size sid s cm
    nodeWithoutFile <- lookupNearestPortWithSpace size sid s cm 
    let mbnp = maybe nodeWithoutFile (\np -> Just np) nodeWithFile
{-  debugM localLogger $ "lookupNearestPortForFile: file:            " ++ show f    
    debugM localLogger $ "lookupNearestPortForFile: size:            " ++ show size
    debugM localLogger $ "lookupNearestPortForFile: site:            " ++ show sid    
    debugM localLogger $ "lookupNearestPortForFile: nodeWithFile:    " ++ show nodeWithFile
    debugM localLogger $ "lookupNearestPortForFile: nodeWithoutFile: " ++ show nodeWithoutFile
    debugM localLogger $ "lookupNearestPortForFile: result:          " ++ show mbnp
-}  return mbnp
    

getOtherFilePorts :: S.FileId -> IdType -> Server -> FileControllerData -> IO [ClientInfo]
getOtherFilePorts f nid s cm
  = do
    let fnm = cm_FileToNodeMap cm
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

instance C.ControllerClass ControllerData where

  closeController cd 
    = do
      debugM localLogger "closing Server"
      closeServer (cd_Server cd)
      debugM localLogger "server closed"


  
  -- getFileSites :: S.FileId -> Controller -> IO (Set.Set SiteId) 
  getFileSites f cd
    = withMVar (cd_FileController cd) $
        \fc ->
        do
        dats <- getFileClientInfoList f (cd_Server cd) fc 
        let sids = map (\ci -> ci_Site ci) dats
        return (Set.fromList sids)

  
  -- containsFile :: S.FileId -> Controller -> IO Bool
  containsFile f cd
    = withMVar (cd_FileController cd) $
        \fc -> return $ Map.member f (cm_FileToNodeMap fc)
  
      
  -- getNearestNodePortWithFile :: S.FileId -> SiteId -> c -> IO (Maybe M.NodeRequestPort)
  getNearestNodePortWithFile f sid cd
    = withMVar (cd_FileController cd) $
        \fc -> lookupNearestPortWithFile f sid (cd_Server cd) fc 

 
  -- getNearestNodePortForFile :: S.FileId -> Integer -> SiteId -> c -> IO (Maybe M.NodeRequestPort)
  getNearestNodePortForFile f c sid cd
    = withMVar (cd_FileController cd) $
        \fc -> lookupNearestPortForFile f c sid (cd_Server cd) fc


-- ----------------------------------------------------------------------------
-- used by the nodes
-- ----------------------------------------------------------------------------


--createFile :: S.FileId -> M.NodeId -> ControllerData -> IO ControllerData
  createFile f nid cd
    = modifyMVar (cd_FileController cd) $
        \fc ->
        do
        let fc' = addFileToController f nid fc
        --TODO copy file
        return (fc', ())


--appendFile :: S.FileId -> M.NodeId -> ControllerData -> IO ControllerData
--TODO
  appendFile f nid cd = C.createFile f nid cd


--deleteFile :: S.FileId -> M.NodeId -> ControllerData -> IO ControllerData
  deleteFile f nid cd
    = modifyMVar (cd_FileController cd) $
        \fc ->
        do
        -- inform all other nodes to delete node
        cps <- getOtherFilePorts f nid (cd_Server cd) fc
        let nps = map (\ci -> NP.newNodePort (ci_Port ci)) cps
        deleteFileFromNodes f nps
        -- delete file from Controller
        let fc' = deleteFileFromController f fc        
        return (fc', ())
        
        

instance Debug ControllerData where
  printDebug cd
    = do
      putStrLn "Controller-Object (full)"
      putStrLn "--------------------------------------------------------"        
      putStrLn "Server"
      printDebug (cd_Server cd)
      putStrLn "--------------------------------------------------------"
      putStrLn "FileToNodeMap:"
      withMVar (cd_FileController cd) $
        \fc -> do
        putStrLn $ show (cm_FileToNodeMap $ fc) 
        