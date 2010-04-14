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

import           Data.Maybe
import qualified Data.List                              as List
import qualified Data.Map                               as Map
import qualified Data.Set                               as Set

import           System.Log.Logger

import           Holumbus.Common.Debug
import           Holumbus.Common.Utils                  ( handleAll )

import qualified Holumbus.FileSystem.Controller         as C
import qualified Holumbus.FileSystem.Node               as N
import qualified Holumbus.FileSystem.Node.NodePort      as NP
import qualified Holumbus.FileSystem.Messages           as M
import qualified Holumbus.FileSystem.Storage            as S

import           Holumbus.Network.Site
import           Holumbus.Network.Communication


import Control.Monad (foldM)
import System.Random


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
      (M.CReqGetNearestNodePortWithFiles l sid) ->
        do
        portmap <- C.getNearestNodePortWithFiles l sid cd
        return $ Just $ M.CRspGetNearestNodePortWithFiles portmap
      (M.CReqGetNearestNodePortForFile f l sid) ->
        do
        p <- C.getNearestNodePortForFile f l sid cd
        return $ Just $ M.CRspGetNearestNodePortForFile p
      (M.CReqGetNearestNodePortForFiles l sid) ->
        do
        p <- C.getNearestNodePortForFiles l sid cd
        return $ Just $ M.CRspGetNearestNodePortForFiles p
      (M.CReqCreate f n) ->
        do
        C.createFile f n cd
        return $ Just $ M.CRspSuccess
      (M.CReqCreateS l) ->
        do
        C.createFiles l cd
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
getFilesForCopying :: FileControllerData -> [S.FileId]
getFilesForCopying cm = (fst . unzip) filteredList 
  where
    fnm = cm_FileToNodeMap cm
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
--addFileToController fid nid cm = addFilesToController [fid] nid cm
addFileToController fid nid cm = cm { cm_FileToNodeMap = fnm' }
  where
    fnm = cm_FileToNodeMap cm
    fnm' = Map.insert fid nid' fnm 
    nid' = Set.singleton nid


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

shuffle :: [a] -> IO [a]
shuffle l' = shuffle' l' []
  where
    shuffle' [] acc = return acc
    shuffle' l acc = do
      k <- randomRIO (0, length l - 1)
      let (lead, x:xs) = splitAt k l
      shuffle' (lead ++ xs) (x:acc)

lookupNearestPortWithFile :: S.FileId -> SiteId -> Server -> FileControllerData -> IO (Maybe ClientPort)
lookupNearestPortWithFile f sid s cm
  = do
    dats <- getFileClientInfoList f s cm
    let sids'  = map (\ci -> ci_Site ci) dats
    sids <- shuffle sids'
    let mbns  = nearestId sid sids
        mbdat = maybe Nothing (\ns -> List.find (\ci -> (ci_Site ci) == ns) dats) mbns
        mbnp  = maybe Nothing (\ci -> Just $ ci_Port ci) mbdat 
    return mbnp

-- | gets the List of all sites the files are located on...
{-
trude :: SiteId -> [S.FileId] -> Server -> FileControllerData -> IO (Maybe SiteId)
trude currentSite files s cm
  = do
    let file2node       = cm_FileToNodeMap cm;
        nodeIdsWithFiles = concatMap (\file -> Set.toList $ maybe Set.empty id (Map.lookup file file2node)) files
    sitesIdsWithFiles <- foldM f ([],[],[]) nodeIdsWithFiles
    nearestId' sitesIdsWithFiles
    where

    nearestId' :: ([SiteId],[SiteId],[SiteId]) -> IO (Maybe SiteId)
    nearestId' ([],  [],  [])  = return Nothing                      -- no site id found
    nearestId' ([],  [],  xs)  = do 
      xs' <- shuffle xs
      return . Just . head $ xs' -- only others ids, shuffle and return the first
    nearestId' ([],  x:_, _)   = return $ Just x                     -- return the hosts stie
    nearestId' (x:_, _,   _)   = return $ Just x                     -- return the procs site

    f :: ([SiteId],[SiteId],[SiteId]) -> M.NodeId -> IO ([SiteId],[SiteId],[SiteId])
    f (procs,hosts,others) i = do
      cli <- getClientInfo i s
      case cli of
        Nothing -> return (procs,hosts,others)
        (Just clientInfo) -> insert (procs,hosts,others) (ci_Site clientInfo)
    
    insert :: ([SiteId],[SiteId],[SiteId]) -> SiteId -> IO ([SiteId],[SiteId],[SiteId])
    insert (procs,hosts,others) thisSite = if isSameProcess thisSite currentSite
                       then return (thisSite:procs,hosts,others)
                       else if isSameHost thisSite currentSite
                              then return (procs,thisSite:hosts,others)
                              else return (procs,hosts,thisSite:others)
-}

lookupNearestPortWithFiles :: [S.FileId] -> SiteId -> Server -> FileControllerData -> IO M.ClientPortMap
lookupNearestPortWithFiles l sid s cm = do
  infoM localLogger $  "Getting nearest ports with: " ++ show l
  res <- foldM f [] l
  infoM localLogger $  "Clientportmap is: " ++ show res
  return res
  where

  f :: M.ClientPortMap -> S.FileId -> IO M.ClientPortMap
  f theMap fid = do

    infoM localLogger $  "Getting nearest ports with: " ++ fid
    maybeport <- lookupNearestPortWithFile fid sid s cm

    debugM localLogger $  "Nearest ports: " ++ show maybeport
    case maybeport of
      (Just port) -> return (ins port fid theMap)
      Nothing -> return theMap

-- lookupNearestPortWithFileAndSpace :: S.FileId -> Integer -> SiteId -> Server -> FileControllerData -> IO (Maybe ClientPort)
-- lookupNearestPortWithFileAndSpace f _size sid s cm
--   = lookupNearestPortWithFile f sid s cm
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
lookupNearestPortWithSpace _size sid s _cm
  = do
    dats <- getAllClientInfos s 
    let sids'  = map (\ci -> ci_Site ci) dats
    sids <- shuffle sids'
    let mbns  = nearestId sid sids
        mbdat = maybe Nothing (\ns -> List.find (\ci -> (ci_Site ci) == ns) dats) mbns
        mbnp  = maybe Nothing (\ci -> Just $ ci_Port ci) mbdat 
    return mbnp


lookupPortWithoutFile :: S.FileId -> Server -> FileControllerData -> IO (Maybe ClientPort)
lookupPortWithoutFile f s cm
  = do
    -- get all sites with the files
    fileCis <- getFileClientInfoList f s cm
    -- get all sites
    allCis  <- getAllClientInfos s
    let fileNids = map ci_Id fileCis
        allNids  = map ci_Id allCis
        nonNids  = Set.toList $ Set.difference (Set.fromList allNids) (Set.fromList fileNids)
    if (null nonNids)
      then return Nothing
      else do
        let i = head nonNids
        mbCi <- getClientInfo i s
        case mbCi of
          (Just ci) -> return $ Just $ ci_Port ci
          (Nothing) -> return Nothing 


lookupNearestPortForFile :: S.FileId -> Integer -> SiteId -> Server -> FileControllerData -> IO (Maybe ClientPort)
lookupNearestPortForFile _ size sid s cm
  -- if file exists, get nearest node, else the closest with space
  = do
--    nodeWithFile    <- lookupNearestPortWithFileAndSpace f size sid s cm
    nodeWithoutFile <- lookupNearestPortWithSpace size sid s cm   
    let mbnp = maybe Nothing (\np -> Just np) nodeWithoutFile
{-  debugM localLogger $ "lookupNearestPortForFile: file:            " ++ show f    
    debugM localLogger $ "lookupNearestPortForFile: size:            " ++ show size
    debugM localLogger $ "lookupNearestPortForFile: site:            " ++ show sid    
    debugM localLogger $ "lookupNearestPortForFile: nodeWithFile:    " ++ show nodeWithFile
    debugM localLogger $ "lookupNearestPortForFile: nodeWithoutFile: " ++ show nodeWithoutFile
    debugM localLogger $ "lookupNearestPortForFile: result:          " ++ show mbnp
-}  return mbnp
    

{-  = do
    dats <- getAllClientInfos s :: [ClientInfo]
    let sids'  = map (\ci -> ci_Site ci) dats
    sids <- shuffle sids'
    let mbns  = nearestId sid sids
        mbdat = maybe Nothing (\ns -> List.find (\ci -> (ci_Site ci) == ns) dats) mbns
        mbnp  = maybe Nothing (\ci -> Just $ ci_Port ci) mbdat
    return mbnp-}

lookupNearestPortForFiles :: [(S.FileId,Integer)] -> SiteId -> Server -> FileControllerData -> IO M.ClientPortMap
lookupNearestPortForFiles l sid s cm = do
  nearestPortWithSpace <- lookupNearestPortWithSpace 0 sid s cm
  case nearestPortWithSpace of
    Nothing -> return []
    (Just p) -> return [(p,map fst l)]
--  infoM localLogger $  "Getting nearest ports for: " ++ show l  
--  res <- foldM f [] l
--  infoM localLogger $  "Clientportmap is: " ++ show res
--  return res
--  where
--
--  f :: M.ClientPortMap -> (S.FileId,Integer) -> IO M.ClientPortMap
--  f theMap (fid,len) = do
--    infoM localLogger $  "Getting nearest ports for: " ++ fid
--    mp <- lookupNearestPortForFile fid len sid s cm
--    debugM localLogger $  "Nearest ports: " ++ show mp
--    cpm <- case mp of
--      (Just port) -> return (ins port fid theMap)
--      Nothing -> return theMap
--    return cpm

ins  :: ClientPort -> S.FileId -> M.ClientPortMap -> M.ClientPortMap
ins port fid []            = [(port,[fid])]
ins port fid ((p,fids):[]) = if (p==port)
                               then [(p,(fid:fids))]
                               else [(port,[fid]),(p,fids)]
ins port fid ((p,fids):ps) = if (p==port)
                               then ((p,fid:fids):ps) 
                               else (p,fids):(ins port fid ps)
  --   

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
        handleAll (\e -> putStrLn $ show e) $
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

  -- getNearestNodePortWithFiles :: [S.FileId] -> SiteId -> c -> IO (Maybe M.NodeRequestPort)
  getNearestNodePortWithFiles l sid cd
    = withMVar (cd_FileController cd) $
        \fc -> lookupNearestPortWithFiles l sid (cd_Server cd) fc 

  -- getNearestNodePortForFile :: S.FileId -> Integer -> SiteId -> c -> IO (Maybe M.NodeRequestPort)
  getNearestNodePortForFile f c sid cd
    = withMVar (cd_FileController cd) $
        \fc -> lookupNearestPortForFile f c sid (cd_Server cd) fc

  -- getNearestNodePortForFiles :: [(S.FileId,Integer)] -> SiteId -> c -> IO (ClientPortMap)
  getNearestNodePortForFiles l sid cd
    = withMVar (cd_FileController cd) $
        \fc -> lookupNearestPortForFiles l sid (cd_Server cd) fc
-- ----------------------------------------------------------------------------
-- used by the nodes
-- ----------------------------------------------------------------------------


--createFile :: S.FileId -> M.NodeId -> ControllerData -> IO ControllerData
  createFile f nid cd
    = modifyMVar (cd_FileController cd) $
        \fc ->
        do
        mbCi <- getClientInfo nid (cd_Server cd)
        case mbCi of
          (Just _) ->
            do
            let fc' = addFileToController f nid fc
            -- copy file to one other node
            mpCp <- lookupPortWithoutFile f (cd_Server cd) fc
            case mpCp of
              (Just _) -> 
                do 
                return ()
                -- let np = NP.newNodePort cp
                -- N.copyFile f (ci_Port ci) np
              (Nothing) -> return ()                  
            return (fc', ())
          (Nothing) -> return (fc,())

--createFiles :: [(S.FileId,M.NodeId)] -> ControllerData -> IO ControllerData
  createFiles l cd
    = modifyMVar (cd_FileController cd) $
        \fc ->
        do
        fc'' <- foldM f fc l
        return  (fc'',())
        where
        f :: FileControllerData -> (S.FileId,M.NodeId) -> IO FileControllerData
        f filecontroller (fid,nid) = do
          mbCi <- getClientInfo nid (cd_Server cd)
          case mbCi of
            (Just _) ->
              do
              let fc' = addFileToController fid nid filecontroller
            -- copy file to one other node
--            mpCp <- lookupPortWithoutFile f (cd_Server cd) fc
--            case mpCp of
--              (Just _) ->
--                do
--                return ()
                -- let np = NP.newNodePort cp
                -- N.copyFile f (ci_Port ci) np
--              (Nothing) -> return ()
              return fc'
            (Nothing) -> return filecontroller


--appendFile :: S.FileId -> M.NodeId -> ControllerData -> IO ControllerData
  appendFile f nid cd
    = modifyMVar (cd_FileController cd) $
        \fc ->
        do
        mbCi <- getClientInfo nid (cd_Server cd)
        case mbCi of
          (Just ci) ->
            do
            -- renew file entry
            let fc' = addFileToController f nid fc
            -- get other nodes with this file
            cps <- getOtherFilePorts f nid (cd_Server cd) fc
            let nps = map (\i -> NP.newNodePort (ci_Port i)) cps
            -- order them to copy the file from the first node
            _ <- mapM (N.copyFile f (ci_Port ci)) nps
            return (fc', ())
          (Nothing) -> return (fc,())


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
  getDebug cd
    = do
      let line = "--------------------------------------------------------"
      tmp <- getDebug (cd_Server cd)
      tmp2 <- withMVar (cd_FileController cd) $
          \fc -> do
          return $ show (cm_FileToNodeMap $ fc)
      return ( "Controller-Object (full)"
        ++"\n"++ line
        ++"\n"++ "Server"
        ++"\n"++ tmp
        ++"\n"++ line
        ++"\n"++ "FileToNodeMap:"
        ++"\n"++tmp2++"\n")
        
