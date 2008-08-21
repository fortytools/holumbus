-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.FileSystem.Node.NodeData
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}

-- ----------------------------------------------------------------------------

module Holumbus.FileSystem.Node.NodeData
(
-- * data types
  NodeData -- reexport from Data

-- * creation and destruction
, newNode
)
where

import           Prelude hiding (appendFile)

import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Monad
import Data.Maybe
import           System.IO hiding (appendFile)
import           System.Log.Logger

import           Holumbus.Common.Utils
import           Holumbus.FileSystem.Node
import qualified Holumbus.FileSystem.Controller as C
import qualified Holumbus.FileSystem.Controller.ControllerPort as CP
import qualified Holumbus.FileSystem.Messages as M
import qualified Holumbus.FileSystem.Storage as S
import qualified Holumbus.FileSystem.Storage.FileStorage as FS

import           Holumbus.Network.Site
import           Holumbus.Network.Port
import           Holumbus.Network.Messages

localLogger :: String
localLogger = "Holumbus.FileSystem.Node.NodeData"


-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

      
-- | Node datatype.
data NodeData = NodeData {
    nd_NodeId         :: MVar (Maybe M.NodeId)
  , nd_SiteId         :: SiteId
  , nd_ServerThreadId :: MVar (Maybe ThreadId)
  , nd_OwnStream      :: M.NodeRequestStream
  , nd_OwnPort        :: M.NodeRequestPort
  , nd_ControllerPort :: MVar CP.ControllerPort
  , nd_Storage        :: MVar FS.FileStorage
  }


-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

--getRequestPortFromNode :: NodeData -> M.NodeRequestPort
--getRequestPortFromNode (External p) = p
--getRequestPortFromNode (Internal _ _ p _ _) = p

newNode :: CP.ControllerPort -> FS.FileStorage -> IO NodeData
newNode cp s
  = do
    -- open storage
    s' <- S.openStorage s
    -- initialize values
    nidMVar <- newMVar Nothing
    sid     <- getSiteId
    tid     <- newMVar Nothing
    st      <- (newLocalStream Nothing::IO M.NodeRequestStream)
    po      <- newPortFromStream st
    sMVar   <- newMVar s'
    cMVar   <- newMVar cp            
    let nd' = (NodeData nidMVar sid tid st po cMVar sMVar)
    -- first, we start the server, because we can't handle requests without it
    startRequestDispatcher tid st (dispatch nd')
    -- then we try to register a the server
    nd  <- registerNode nd'
    return nd


dispatch 
  :: NodeData 
  -> M.NodeRequestMessage 
  -> M.NodeResponsePort
  -> IO ()
dispatch nd msg replyPort
  = do
    case msg of
      (M.NReqCreate i c) ->
        do
        handleRequest replyPort (createFile i c nd) (\_ -> M.NRspSuccess)
        return ()
      (M.NReqAppend i c) -> 
        do
        handleRequest replyPort (appendFile i c nd) (\_ -> M.NRspSuccess)
        return ()
      (M.NReqDelete i b) ->
        do
        handleRequest replyPort (deleteFile i b nd) (\_ -> M.NRspSuccess)
        return ()
      (M.NReqContains i)-> 
        do
        handleRequest replyPort (containsFile i nd) (\b -> M.NRspContains b)
        return ()
      (M.NReqGetFileContent i) ->
        do
        handleRequest replyPort (getFileContent i nd) (\c -> M.NRspGetFileContent c)
        return ()
      (M.NReqGetFileData i) ->
        do
        handleRequest replyPort (getFileData i nd) (\d -> M.NRspGetFileData d)       
        return ()
      (M.NReqGetFileIds) ->
        do
        handleRequest replyPort (getFileIds nd) (\ls -> M.NRspGetFileIds ls)       
        return ()
      _ -> 
        handleRequest replyPort (return ()) (\_ -> M.NRspUnknown)



-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


registerNode :: NodeData -> IO (NodeData)
registerNode nd
  = do
    debugM localLogger "registering at controller"
    let sid = (nd_SiteId nd)
    let np = (nd_OwnPort nd)
    -- get the new nid
    -- TODO think about this
    E.catch
     (do
      nid <- withMVar (nd_ControllerPort nd) $
        \cp ->
        do  
        (nid, _) <- C.registerNode sid np cp
        return (Just nid)
      -- write the new nodeId in the record
      modifyMVar (nd_NodeId nd) (\_ -> return (nid,nd)) 
     )
     (\e -> 
      do 
      errorM localLogger $ show e
      return nd
     )   

unregisterNode :: NodeData -> IO (NodeData)
unregisterNode nd
  = do
    debugM localLogger "unregistering at controller"
    nid <- readMVar (nd_NodeId nd)
    unregister nid
    modifyMVar (nd_NodeId nd) (\_ -> return (Nothing,nd))
    where
      unregister Nothing = return ()
      unregister (Just i)
        = do        
          withMVar (nd_ControllerPort nd) $
            \cp -> C.unregisterNode i cp
          return ()



-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


changeStorage :: (FS.FileStorage -> IO FS.FileStorage) -> NodeData -> IO NodeData
changeStorage f nd
  = do
    -- handle (\e -> return (Left $ show e)) $
    --   do
    modifyMVar_ (nd_Storage nd) f
    return nd



readStorage :: (FS.FileStorage-> IO a) -> NodeData -> IO a
readStorage f nd
  = do
    --handle (\e -> return (Left $ show e)) $
    --  do
    res <- withMVar (nd_Storage nd) f
    return res



-- ----------------------------------------------------------------------------
-- Typeclass instanciation (NodeClass)
-- ----------------------------------------------------------------------------


instance Node NodeData where
  

  closeNode nd
    = do
      -- shutdown the server thread and the stream
      nd'  <- unregisterNode nd
      stopRequestDispatcher (nd_ServerThreadId nd')
      closeStream (nd_OwnStream nd')
      return ()
    
     
  getNodeRequestPort = nd_OwnPort   
  
     
  --createFile :: S.FileId -> S.FileContent -> NodeData -> IO NodeData
  createFile i c nd 
    = do
      nid <- readMVar (nd_NodeId nd)
      case nid of
        (Nothing) ->
          do
          --TODO besser exception-handling
          putStrLn "ERROR: Node.createFile - no nid"
          return nd
        (Just nid') ->
          do
          changeStorage (\stor -> S.createFile stor i c) nd
          withMVar (nd_ControllerPort nd) $
            \cp ->
            do
            C.createFile i nid' cp
            return nd


  --appendFile :: S.FileId -> S.FileContent -> NodeData -> IO NodeData
  appendFile i c nd
    = do
      nid <- readMVar (nd_NodeId nd)
      case nid of
        (Nothing) ->
          do
          --TODO besser exception-handling
          putStrLn "ERROR: Node.createFile - no nid"
          return nd
        (Just nid') ->
          do
          changeStorage (\stor -> S.appendFile stor i c) nd
          withMVar (nd_ControllerPort nd) $
            \cp ->
            do
            C.appendFile i nid' cp
            return nd      


  --deleteFile :: S.FileId -> NodeData -> IO NodeData
  deleteFile i b nd
    = do
      nid <- readMVar (nd_NodeId nd)
      case nid of
        (Nothing) ->
          do
          --TODO besser exception-handling
          putStrLn "ERROR: Node.createFile - no nid"
          return nd
        (Just nid') ->
          do
          changeStorage (\stor -> S.deleteFile stor i) nd
          if (b) 
            then do
              withMVar (nd_ControllerPort nd) $
                \cp ->
                do
                C.deleteFile i nid' cp
                return ()
            else return ()
          return nd      


  --containsFile :: S.FileId -> NodeData -> IO Bool
  containsFile i nd
    = do
      readStorage (\stor -> S.containsFile stor i) nd


  --getFileContent :: S.FileId -> NodeData -> IO (Maybe S.FileContent)
  getFileContent i nd
    = do
      c <- readStorage (\stor -> S.getFileContent stor i) nd
      debugM localLogger $ "getFileContent: " ++ show c
      return c
    

  --getFileData :: S.FileId -> NodeData -> IO (Maybe S.FileData)
  getFileData i nd
    = do
      readStorage (\stor -> S.getFileData stor i) nd


  --getFileIds :: NodeData -> IO [S.FileId]
  getFileIds nd
    = do
      readStorage (\stor -> S.getFileIds stor) nd


  printDebug nd
    = do
      putStrLn "Node-Object (full)"
      withMVar (nd_NodeId nd) $
        \nid -> putStrLn $ prettyRecordLine gap "NodeId:" nid
      putStrLn $ prettyRecordLine gap "SiteId:" (nd_SiteId nd)
      putStrLn $ prettyRecordLine gap "OwnStream:" (nd_OwnStream nd)
      putStrLn $ prettyRecordLine gap "OwnPort:" (nd_OwnPort nd)
      withMVar (nd_ServerThreadId nd) $
        \i -> do putStrLn $ prettyRecordLine gap "ServerId:" i
      withMVar (nd_ControllerPort nd) $
        \cp -> do putStrLn $ prettyRecordLine gap "ControllerPort:" cp
      withMVar (nd_Storage nd) $
        \s -> do putStrLn $ prettyRecordLine gap "Storage:" s
      where
        gap = 20
      