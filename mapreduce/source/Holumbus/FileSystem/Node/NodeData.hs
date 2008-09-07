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
      (M.NReqAppend i c) -> 
        do
        appendFile i c nd
        return $ Just M.NRspSuccess
      (M.NReqDelete i b) ->
        do
        deleteFile i b nd
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
  = withMVar node $ \nd -> f (nd_Storage nd)



-- ----------------------------------------------------------------------------
-- Typeclass instanciation (NodeClass)
-- ----------------------------------------------------------------------------


instance NodeClass Node where
  

  closeNode (Node node)
    = modifyMVar node $
        \nd ->
        do
        debugM localLogger "closing filesystem node"
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
          --TODO besser exception-handling
          putStrLn "ERROR: Node.createFile - no nid"
          return ()
        (Just nid') ->
          do
          changeStorage (\stor -> S.createFile stor i c) n 
          server <- getServerPort client
          let cp = CP.newControllerPortFromServerPort server
          C.createFile i nid' cp
          return ()
          

  --appendFile :: S.FileId -> S.FileContent -> Node -> IO ()
  appendFile i c n@(Node node)
    = do
      client <- withMVar node $ \nd -> return (nd_Client nd)
      nid <- getClientId client
      case nid of
        (Nothing) ->
          do
          --TODO besser exception-handling
          putStrLn "ERROR: Node.createFile - no nid"
          return ()
        (Just nid') ->
          do
          changeStorage (\stor -> S.appendFile stor i c) n
          server <- getServerPort client
          let cp = CP.newControllerPortFromServerPort server
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
          --TODO besser exception-handling
          putStrLn "ERROR: Node.createFile - no nid"
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


  --containsFile :: S.FileId -> Node -> IO Bool
  containsFile i n
    = do
      readStorage (\stor -> S.containsFile stor i) n


  --getFileContent :: S.FileId -> Node -> IO (Maybe S.FileContent)
  getFileContent i n
    = do
      c <- readStorage (\stor -> S.getFileContent stor i) n
      debugM localLogger $ "getFileContent: " ++ show c
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
