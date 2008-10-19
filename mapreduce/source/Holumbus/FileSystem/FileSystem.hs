-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.FileSystem.FileSystem
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts #-}
module Holumbus.FileSystem.FileSystem
(
-- * Datatypes
  S.FileId
, S.FileContent
, S.getContentLength
, S.FileData(..)
, FileSystem

-- * Configuration
, FSStandaloneConf(..)
, defaultFSStandaloneConfig
, FSControllerConf(..)
, defaultFSControllerConfig
, FSNodeConf(..)
, defaultFSNodeConfig
, FSClientConf(..)
, defaultFSClientConfig

-- * Creation and Destruction
, mkStandaloneFileSystem
, mkFileSystemController
, mkFileSystemNode
, mkFileSystemClient
, closeFileSystem

-- * Operations
, getMySiteId
, getFileSites
, getNearestNodePortWithFile
, getNearestNodePortForFile
, containsFile

, createFile
, appendFile
, deleteFile

, getFileContent
, getFileData
, isFileLocal

)
where

import           Prelude hiding (appendFile)
import           Control.Concurrent
import           Control.Monad
import qualified Data.Set as Set
import           System.Log.Logger

import           Holumbus.Common.Debug
import qualified Holumbus.FileSystem.Controller as C
import qualified Holumbus.FileSystem.Controller.ControllerData as CD
import qualified Holumbus.FileSystem.Controller.ControllerPort as CP
import qualified Holumbus.FileSystem.Node as N
import qualified Holumbus.FileSystem.Node.NodeData as ND
import qualified Holumbus.FileSystem.Node.NodePort as NP
import qualified Holumbus.FileSystem.Storage as S
import qualified Holumbus.FileSystem.Storage.FileStorage as FST
import           Holumbus.Network.Site
import           Holumbus.Network.Communication


localLogger :: String
localLogger = "Holumbus.FileSystem.FileSystem"
 
-- ---------------------------------------------------------------------------
-- Datatypes
-- ---------------------------------------------------------------------------

data FileSystemData = 
   forall c n. (C.ControllerClass c, N.NodeClass n, Debug c, Debug n) =>
   FileSystemData SiteId c (Maybe n)
--    fsd_siteId     :: SiteId
--  , fsd_controller :: MVar c
--  , fsd_nodep      :: forall n. (N.Node n) => MVar (Maybe n)
--}

data FileSystem = FileSystem (MVar FileSystemData)

instance Show FileSystem where
  show _ = "FileSystem"



-- ---------------------------------------------------------------------------
-- Configurations
-- ---------------------------------------------------------------------------


data FSStandaloneConf = FSStandaloneConf {
    fstc_StreamName       :: StreamName
  , fstc_StoragePath      :: FilePath
  , fstc_StorageFile      :: FilePath
  } 
  
defaultFSStandaloneConfig :: FSStandaloneConf
defaultFSStandaloneConfig = FSStandaloneConf "FSController" "storage/" "directory"


data FSControllerConf = FSControllerConf {
    fcoc_StreamName       :: StreamName
  , fcoc_PortNumber       :: Maybe PortNumber
  }
   
defaultFSControllerConfig :: FSControllerConf
defaultFSControllerConfig = FSControllerConf "FSController" Nothing


data FSNodeConf = FSNodeConf {
    fnoc_StreamName       :: StreamName
  , fnoc_SocketId         :: Maybe SocketId
  , fnoc_StoragePath      :: FilePath
  , fnoc_StorageFile      :: FilePath
  }
   
defaultFSNodeConfig :: FSNodeConf
defaultFSNodeConfig = FSNodeConf "FSController" Nothing "storage/" "directory"


data FSClientConf = FSClientConf {
    fclc_StreamName       :: StreamName
  , fclc_SocketId         :: Maybe SocketId
  }
   
defaultFSClientConfig :: FSClientConf
defaultFSClientConfig = FSClientConf "FSController" Nothing




-- ---------------------------------------------------------------------------
-- Creation and Destruction
-- ---------------------------------------------------------------------------


mkStandaloneFileSystem
  :: FSStandaloneConf 
  -> IO (FileSystem)
mkStandaloneFileSystem conf
  = do
    controller <- CD.newController (fstc_StreamName conf) Nothing
    let storage = FST.newFileStorage (fstc_StoragePath conf) (fstc_StorageFile conf) 
    node <- ND.newNode (fstc_StreamName conf) Nothing storage
    fs <- newFileSystem controller (Just node)
    return fs


mkFileSystemController
  :: FSControllerConf
  -> IO (FileSystem)
mkFileSystemController conf
  = do
    sid <- getSiteId
    infoM localLogger $ "initialising controller on site " ++ show sid  
    controller <- CD.newController (fcoc_StreamName conf) (fcoc_PortNumber conf)
    newFileSystem controller (Nothing::Maybe NP.NodePort)
  

mkFileSystemNode
  :: FSNodeConf
  -> IO (FileSystem)
mkFileSystemNode conf
  = do
    sid <- getSiteId
    infoM localLogger $ "initialising node on site " ++ show sid  
    cp <- CP.newControllerPort (fnoc_StreamName conf) (fnoc_SocketId conf)
    infoM localLogger "creating filestorage"
    let storage = FST.newFileStorage (fnoc_StoragePath conf) (fnoc_StorageFile conf) 
    node <- ND.newNode (fnoc_StreamName conf) (fnoc_SocketId conf) storage
    newFileSystem cp (Just node)
  

mkFileSystemClient
  :: FSClientConf
  -> IO (FileSystem)
mkFileSystemClient conf
  = do
    sid <- getSiteId
    infoM localLogger $ "initialising client on site " ++ show sid  
    cp <- CP.newControllerPort (fclc_StreamName conf) (fclc_SocketId conf)
    newFileSystem cp (Nothing::Maybe NP.NodePort)


-- | Closes the filesystem.
closeFileSystem :: FileSystem -> IO ()
closeFileSystem (FileSystem fs) 
  = do
    debugM localLogger "beginning to close filesystem"
    withMVar fs $
      \(FileSystemData _ c n) ->
      do
      case n of
        (Just n') -> 
          do
          debugM localLogger "closing Node"
          N.closeNode n'
        (Nothing) -> 
          do
          debugM localLogger "no Node to close"
          return ()
      debugM localLogger "closing Controller"
      C.closeController c
      debugM localLogger "filesystem closed"



-- ---------------------------------------------------------------------------
-- private helper-functions
-- ---------------------------------------------------------------------------


-- | Creates a new FileSystem with a controller and (maybe) a node.
newFileSystem :: (C.ControllerClass c, N.NodeClass n, Debug c, Debug n) => c -> Maybe n -> IO (FileSystem)
newFileSystem c n
  = do
    sid <- getSiteId
    fs <- newMVar (FileSystemData sid c n)
    return (FileSystem fs)


-- | get a NodePort from a NodeRequestPort
createNodePort :: (Maybe ClientPort) -> (Maybe NP.NodePort)
createNodePort Nothing = Nothing
createNodePort (Just p) = Just $ NP.newNodePort p


-- ---------------------------------------------------------------------------
-- public functions
-- ---------------------------------------------------------------------------

getMySiteId :: FileSystem -> IO (SiteId)
getMySiteId (FileSystem fs)
  = do 
    withMVar fs $
      \(FileSystemData s _ _) -> return s

-- | Get a set of all sites the file exists.
getFileSites :: S.FileId -> FileSystem -> IO (Set.Set SiteId)
getFileSites f (FileSystem fs)
  = do
    withMVar fs $
      \(FileSystemData _ c _) -> 
      C.getFileSites f c


-- | gets the nearest NodePort with our fileId
getNearestNodePortWithFile :: S.FileId -> FileSystem -> IO (Maybe NP.NodePort)
getNearestNodePortWithFile f (FileSystem fs)
  = withMVar fs $
      \(FileSystemData sid c _) -> 
      (liftM createNodePort) $ C.getNearestNodePortWithFile f sid c
    

-- | gets the nearest NodePort on which we can create our fileId. we need the
--   content-size to get a node with enough space.
getNearestNodePortForFile :: S.FileId -> Integer -> FileSystem -> IO (Maybe NP.NodePort)
getNearestNodePortForFile f l (FileSystem fs)
  = withMVar fs $
      \(FileSystemData sid c _) -> 
      (liftM createNodePort) $ C.getNearestNodePortForFile f l sid c


-- | Checks if a file is in the filesystem
containsFile :: S.FileId -> FileSystem -> IO Bool
containsFile f (FileSystem fs)
  = do
    withMVar fs $
      \(FileSystemData _ c _) ->
      C.containsFile f c


-- | Creates a file in the filesystem.
createFile :: S.FileId -> S.FileContent -> FileSystem -> IO ()
createFile f c fs
  = do
    infoM localLogger $ "creating file: " ++ show f
    np <- getNearestNodePortForFile f (S.getContentLength c) fs
    case np of
      (Nothing) -> return ()
      (Just np') -> 
        do 
        N.createFile f c np'
        return ()


-- | Appends a file in the fileSystem.
appendFile :: S.FileId -> S.FileContent -> FileSystem -> IO ()
appendFile f c fs
  = do
    infoM localLogger $ "appending file: " ++ show f
    np <- getNearestNodePortForFile f (S.getContentLength c) fs
    debugM localLogger $ "nearest port: " ++ show np
    case np of
      (Nothing) -> return ()
      (Just np') ->
        do
        N.appendFile f c np'
        return ()


-- | Deletes a file from the filesystem.
deleteFile :: S.FileId -> FileSystem -> IO ()
deleteFile f fs
  = do
    infoM localLogger $ "deleting file: " ++ show f
    np <- getNearestNodePortWithFile f fs
    case np of
      (Nothing) -> return ()
      (Just np') ->
        do
        -- start a delete cascade... (True)
        N.deleteFile f True np'
        return ()


-- | Gets the file content from the nearest site whitch holds the file
getFileContent :: S.FileId -> FileSystem -> IO (Maybe S.FileContent)
getFileContent f fs
  = do
    infoM localLogger $ "getting file: " ++ show f
    np <- getNearestNodePortWithFile f fs
    case np of
      (Nothing) -> return Nothing
      (Just np') ->
        do
        N.getFileContent f np' 


-- | Gets the file data from the nearest site whitch holds the file
getFileData :: S.FileId -> FileSystem -> IO (Maybe S.FileData)
getFileData f fs 
  = do
    np <- getNearestNodePortWithFile f fs
    case np of
      (Nothing) -> return Nothing
      (Just np') ->
        do
        N.getFileData f np' 


-- | Tests, if the local storage (if one exists) holds the file
isFileLocal :: S.FileId -> FileSystem -> IO Bool
isFileLocal f (FileSystem fs)
  = do
    withMVar fs $
      \(FileSystemData _ _ n) ->
      maybe (return False) (\n' -> N.containsFile f n') n


instance Debug FileSystem where  
  printDebug (FileSystem fs)
    = do
      withMVar fs $
        \(FileSystemData s c n) ->
        do
        putStrLn "--------------------------------------------------------"
        putStrLn "FileSystem - internal data\n"
        putStrLn "--------------------------------------------------------"
        putStrLn "SiteId:"
        putStrLn $ show s
        putStrLn "--------------------------------------------------------"
        putStrLn "Controller:"
        printDebug c
        putStrLn "--------------------------------------------------------"
        putStrLn "Node:"
        maybe (putStrLn "NOTHING") (\n' -> printDebug n') n
        putStrLn "--------------------------------------------------------"