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
  NodeData

-- * creation and destruction
, newNode
)
where

import           Prelude hiding (appendFile)

import           Control.Concurrent
import           Control.Monad
import           Data.Maybe
import           System.IO hiding (appendFile)
import           System.Log.Logger

import           Holumbus.Common.Debug
import           Holumbus.Common.Utils
import           Holumbus.FileSystem.Node
import           Holumbus.FileSystem.Node.NodePort
import qualified Holumbus.FileSystem.Controller as C
import qualified Holumbus.FileSystem.Controller.ControllerPort as CP
import qualified Holumbus.FileSystem.Messages as M
import qualified Holumbus.FileSystem.Storage as S
import qualified Holumbus.FileSystem.Storage.FileStorage as FS

import           Holumbus.Network.Communication

localLogger :: String
localLogger = "Holumbus.FileSystem.Node.NodeData"


-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

      
-- | Node datatype.
data NodeData = NodeData {
    nd_Client          :: Client
  , nd_Storage         :: FS.FileStorage
  }

data Node = Node (MVar NodeData)

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

newNode :: StreamName -> Maybe SocketId -> FS.FileStorage -> IO Node
newNode sn soid stor
  = do
    -- initialise the client
    node <- newEmptyMVar
    client <- newClient sn soid (dispatch (Node node))
    -- open storage
    stor' <- S.openStorage stor             
    putMVar node (NodeData client stor')
    return (Node node)


dispatch 
  :: Node
  -> M.NodeRequestMessage 
  -> IO (Maybe M.NodeResponseMessage)
dispatch nd msg
  = do
    case msg of
      (M.NReqCreate i c) ->
        do
        createFile i c nd
        return $ Just M.NRspSuccess        
      (M.NReqCreateS l) ->
        do
        createFiles l nd
        return $ Just M.NRspSuccess
      (M.NReqAppend i c) -> 
        do
        appendFile i c nd
        return $ Just M.NRspSuccess
      (M.NReqDelete i b) ->
        do
        deleteFile i b nd
        return $ Just M.NRspSuccess
      (M.NReqCopy i cp) ->
        do
        copyFile i cp nd
        return $ Just M.NRspSuccess
      (M.NReqContains i)-> 
        do
        b <- containsFile i nd
        return $ Just $ M.NRspContains b
      (M.NReqGetFileContent i) ->
        do
        c <- getFileContent i nd
        return $ Just $ M.NRspGetFileContent c
      (M.NReqGetFileData i) ->
        do
        d <- getFileData i nd
        return $ Just $ M.NRspGetFileData d
      (M.NReqGetFileIds) ->
        do
        ls <- getFileIds nd
        return $ Just $ M.NRspGetFileIds ls
      _ -> return Nothing 



-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


changeStorage :: (FS.FileStorage -> IO FS.FileStorage) -> Node -> IO ()
changeStorage f (Node node)
  = modifyMVar node $ 
      \nd -> 
      do
      stor' <- f (nd_Storage nd)
      return (nd {nd_Storage = stor'}, ())


readStorage :: (FS.FileStorage-> IO a) -> Node -> IO a
readStorage f (Node node)
  = do
    a <- withMVar node $ \nd -> f (nd_Storage nd)    
    return a

logStorage :: Node -> IO ()
logStorage (Node node) = withMVar node $ \nd ->  logStorage2 (nd_Client nd) 
  where
  logStorage2 (Client client) = withMVar client $ \cd -> debugM "measure.readStorage" ((show . cd_SiteId) cd)
  
-- ----------------------------------------------------------------------------
-- Typeclass instanciation (NodeClass)
-- ----------------------------------------------------------------------------


instance NodeClass Node where
  

  closeNode (Node node)
    = modifyMVar node $
        \nd ->
        do
        infoM localLogger "closing filesystem node"
        closeClient (nd_Client nd)
        return (nd,())
      
     
  --createFile :: S.FileId -> S.FileContent -> Node -> IO ()
  createFile i c n@(Node node)
    = do
      client <- withMVar node $ \nd -> return (nd_Client nd)
      nid <- getClientId client
      case nid of
        (Nothing) ->
          do
          --TODO better exception-handling
          errorM localLogger $ "createFile \"" ++ i ++ "\" - no nid"
          return ()
        (Just nid') ->
          do
          changeStorage (\stor -> S.createFile stor i c) n 
          server <- getServerPort client
          let cp = CP.newControllerPortFromServerPort server
          C.createFile i nid' cp
          return ()
          
--  createFiles l n = mapM_ (\(fid,c) -> createFile fid c n) l
  --createFiles :: [(S.FileId,S.FileContent)] -> Node -> IO ()
  createFiles l n@(Node node)
    = do
      client <- withMVar node $ \nd -> return (nd_Client nd)
      nid <- getClientId client
      case nid of
        (Nothing) ->
          do
          --TODO better exception-handling
          errorM localLogger $ "createFiles - no nid"
          return ()
        (Just nid') ->
          do
          mapM_ (\(i,c) -> changeStorage (\stor -> S.createFile stor i c) n) l
          server <- getServerPort client
          let cp = CP.newControllerPortFromServerPort server
          let fns = map (\(i,_) -> (i,nid')) l
          C.createFiles fns  cp
          return ()


  --appendFile :: S.FileId -> S.FileContent -> Node -> IO ()
  appendFile i c n@(Node node)
    = do
      client <- withMVar node $ \nd -> return (nd_Client nd)
      nid <- getClientId client
      case nid of
        (Nothing) ->
          do
          --TODO better exception-handling          
          errorM localLogger $ "appendFile \"" ++ i ++ "\" - no nid"
          return ()
        (Just nid') ->
          do
          changeStorage (\stor -> S.appendFile stor i c) n
          server <- getServerPort client
          let cp = CP.newControllerPortFromServerPort server
          -- Inform Controller that file has changed.
          C.appendFile i nid' cp          
          return ()


  --deleteFile :: S.FileId -> Node -> IO ()
  deleteFile i b n@(Node node)
    = do
      client <- withMVar node $ \nd -> return (nd_Client nd)
      nid <- getClientId client
      case nid of
        (Nothing) ->
          do
          --TODO better exception-handling
          errorM localLogger $ "deleteFile \"" ++ i ++ "\" - no nid"
          return ()
        (Just nid') ->
          do
          changeStorage (\stor -> S.deleteFile stor i) n
          if (b) 
            then do
              server <- getServerPort client
              let cp = CP.newControllerPortFromServerPort server
              C.deleteFile i nid' cp
              return ()
            else return ()


  copyFile i cp n
    = do
      let np = newNodePort cp
      c <- getFileContent i np
      -- TODO do not create new file data 
      --d <- getFileData i np
      if (isJust c) 
        then do
          createFile i (fromJust c) n
          return ()
        else do
          errorM localLogger $ "copyFile: no content for \"" ++ i ++ "\" found"
          return ()


  --containsFile :: S.FileId -> Node -> IO Bool
  containsFile i n
    = do
      readStorage (\stor -> S.containsFile stor i) n


  --getFileContent :: S.FileId -> Node -> IO (Maybe S.FileContent)
  getFileContent i n
    = do
      c <- readStorage (\stor -> S.getFileContent stor i) n
      debugM localLogger $ "getFileContent: " ++ show c
      logStorage n 
      return c
    

  --getFileData :: S.FileId -> Node -> IO (Maybe S.FileData)
  getFileData i nd
    = do
      readStorage (\stor -> S.getFileData stor i) nd


  --getFileIds :: Node -> IO [S.FileId]
  getFileIds nd
    = do
      readStorage (\stor -> S.getFileIds stor) nd




instance Debug Node where
  printDebug (Node node)
    = do
      putStrLn "Node-Object (full)"
      withMVar node $
        \nd ->
        do
        printDebug (nd_Client nd)
        putStrLn $ prettyRecordLine gap "Storage:" (nd_Storage nd)
        where
        gap = 20
  getDebug (Node node)
    = do      
      tmp <- withMVar node $
        \nd ->
        do
        tmp <- getDebug (nd_Client nd)
        return (tmp++"\n"++ (prettyRecordLine gap "Storage:" (nd_Storage nd)) ++"\n")
      return ("Node-Object (full)"++"\n"++tmp++"\n")
        where
        gap = 20        
