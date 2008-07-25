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
, S.FileContent(..)
, S.getContentLength
, S.FileData(..)
, FileSystem

-- * Creation and Destruction
, standaloneFileSystem
, newFileSystem
, setFileSystemNode
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

-- * debug
, printDebug
)
where

import Prelude hiding (appendFile)

import Control.Concurrent
import Control.Monad

import qualified Data.Set as Set

import Holumbus.Network.Site

import qualified Holumbus.FileSystem.Messages as M
import qualified Holumbus.FileSystem.Controller as C
import qualified Holumbus.FileSystem.Controller.ControllerData as CD
import qualified Holumbus.FileSystem.Controller.ControllerPort as CP
import qualified Holumbus.FileSystem.Node as N
import qualified Holumbus.FileSystem.Node.NodeData as ND
import qualified Holumbus.FileSystem.Node.NodePort as NP
import qualified Holumbus.FileSystem.Storage as S
import qualified Holumbus.FileSystem.Storage.FileStorage as FST


 
-- ---------------------------------------------------------------------------
-- Datatypes
-- ---------------------------------------------------------------------------

data FileSystemData = 
   forall c n. (C.Controller c, N.Node n) =>
   FileSystemData SiteId c (Maybe n)
--    fsd_siteId     :: SiteId
--  , fsd_controller :: MVar c
--  , fsd_nodep      :: forall n. (N.Node n) => MVar (Maybe n)
--}

data FileSystem = FileSystem (MVar FileSystemData)

instance Show FileSystem where
  show _ = "FileSystem"


-- ---------------------------------------------------------------------------
-- Creation and Destruction
-- ---------------------------------------------------------------------------


standaloneFileSystem
  :: FilePath           -- ^ the path of the filestorage on disk 
  -> (Maybe FilePath)   -- ^ the name of the directory file, if "Nothing" the default name will be used
  -> IO (FileSystem)
standaloneFileSystem fp fn
  = do
    controller <- CD.newController
    let storage = FST.newFileStorage fp fn
    let cp = CP.newControllerPort $ C.getControllerRequestPort controller
    n <- ND.newNode cp storage
    fs <- newFileSystem controller
    setFileSystemNode n fs
    return fs

--TODO
-- | Creates a new FileSystem with a controller and (maybe) a node.
{-newFileSystem :: (C.Controller c, N.Node n) => c -> Maybe n -> IO (FileSystem)
newFileSystem c n
  = do
    sid <- getSiteId
    newMVar (FileSystemData sid c n)
-}

newFileSystem :: (C.Controller c) => c -> IO (FileSystem)
newFileSystem c
  = do
    sid <- getSiteId
    fs <- newMVar (FileSystemData sid c emptyNode)
    return (FileSystem fs)
    where
      emptyNode :: Maybe NP.NodePort
      emptyNode = Nothing


setFileSystemNode :: (N.Node n) => n -> FileSystem -> IO ()
setFileSystemNode n (FileSystem fs)
  = do
    modifyMVar fs $
      \(FileSystemData s c _) -> return (FileSystemData s c (Just n), ())


--TODO
-- | Closes the filesystem.
closeFileSystem :: FileSystem -> IO ()
closeFileSystem _ = return ()



-- ---------------------------------------------------------------------------
-- private helper-functions
-- ---------------------------------------------------------------------------


-- | get a NodePort from a NodeRequestPort
createNodePort :: (Maybe M.NodeRequestPort) -> (Maybe NP.NodePort)
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
  = do
    withMVar fs $
      \(FileSystemData sid c _) -> 
      (liftM createNodePort) $ C.getNearestNodePortWithFile f sid c
    

-- | gets the nearest NodePort on which we can create our fileId. we need the
--   content-size to get a node with enough space.
getNearestNodePortForFile :: S.FileId -> Integer -> FileSystem -> IO (Maybe NP.NodePort)
getNearestNodePortForFile f l (FileSystem fs)
  = do
    withMVar fs $
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
    np <- getNearestNodePortForFile f (S.getContentLength c) fs
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


printDebug :: FileSystem -> IO ()
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
      C.printDebug c
      putStrLn "--------------------------------------------------------"
      putStrLn "Node:"
      maybe (putStrLn "NOTHING") (\n' -> N.printDebug n') n
      putStrLn "--------------------------------------------------------"