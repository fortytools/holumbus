-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Network.Communication
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

-}

-- ----------------------------------------------------------------------------


module Holumbus.Network.Communication
(
-- * server operations
  Server
, newServer
, closeServer

-- * client operations
, Client
, newClient
, closeClient
)
where

import           Control.Concurrent
import qualified Control.Exception as E
import           Data.Binary
import qualified Data.Map as Map
import           Data.Maybe
import           Network
import           System.Log.Logger

import           Holumbus.Common.Debug
import           Holumbus.Common.Threading
import qualified Holumbus.Data.MultiMap as MMap
import           Holumbus.Network.Messages
import           Holumbus.Network.Port
import           Holumbus.Network.Site



localLogger :: String
localLogger = "Holumbus.Network.Communication"


-- ----------------------------------------------------------------------------
-- General Datatypes
-- ----------------------------------------------------------------------------

-- | the type of the client id
type IdType = Int



-- ----------------------------------------------------------------------------
-- Server-Messages
-- ----------------------------------------------------------------------------

-- | the requests, the server can handle
data ServerRequestMessage
  = SReqRegisterClient SiteId (Port ClientRequestMessage)
  | SReqUnregisterClient IdType
  | SReqPing IdType
  | SReqUnknown
  deriving (Show)

instance Binary (ServerRequestMessage) where
  put (SReqRegisterClient sid po) = putWord8 1 >> put sid >> put po
  put (SReqUnregisterClient i)    = putWord8 2 >> put i
  put (SReqPing i)                = putWord8 3 >> put i
  put (SReqUnknown)               = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> get >>= \sid -> get >>= \po -> return (SReqRegisterClient sid po)
        2 -> get >>= \i -> return (SReqUnregisterClient i)
        3 -> get >>= \i -> return (SReqPing i)
        _ -> return (SReqUnknown)


-- | the responses the server gives
data ServerResponseMessage
  = SRspSuccess
  | SRspRegisterClient IdType
  | SRspUnregisterClient
  | SRspPing Bool
  | SRspError String
  | SRspUnknown
  deriving (Show)

instance Binary (ServerResponseMessage) where
  put (SRspSuccess)          = putWord8 1
  put (SRspRegisterClient i) = putWord8 2 >> put i
  put (SRspUnregisterClient) = putWord8 3
  put (SRspPing b)           = putWord8 4 >> put b
  put (SRspError e)          = putWord8 5 >> put e
  put (SRspUnknown)          = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> return (SRspSuccess)
        2 -> get >>= \i -> return (SRspRegisterClient i)
        3 -> return (SRspUnregisterClient)
        4 -> get >>= \b -> return (SRspPing b)
        5 -> get >>= \e -> return (SRspError e)
        _ -> return (SRspUnknown)
  
instance RspMsg (ServerResponseMessage) where
  isError (SRspError _) = True
  isError _ = False
  
  getErrorMsg (SRspError e) = e
  getErrorMsg _ = ""
  
  isUnknown (SRspUnknown) = True
  isUnknown _ = False
  
  mkErrorMsg e = SRspError e
  



-- ----------------------------------------------------------------------------
-- Server-TypeClass
-- ----------------------------------------------------------------------------

-- | the request-functions a server has to implement
class ServerClass s where
  registerClient :: SiteId -> Port ClientRequestMessage -> s -> IO IdType
  unregisterClient :: IdType -> s -> IO ()
  pingServer :: IdType-> s -> IO Bool



-- ----------------------------------------------------------------------------
-- Server-Data
-- ----------------------------------------------------------------------------
  
-- the information of the client known by the server
data ClientInfo = ClientInfo {
    ci_Site         :: SiteId                -- ^ SiteId (Hostname,PID) of the client process
  , ci_Port         :: ClientPort            -- ^ the port of the client
  , ci_PingThreadId :: MVar (Maybe ThreadId) -- ^ the threadId of the ping-Process (needed to stop it)
  }
  
instance Show ClientInfo where
  show (ClientInfo s p _) = "{Site: " ++ show s ++ " - Port: " ++ show p ++ "}"


-- | the data of the server needed to organise the clients
data ServerData = ServerData {
    sd_ServerThreadId  :: MVar (Maybe ThreadId)         -- ^ threadId of the streamDispatcher
  , sd_OwnStream       :: Stream (ServerRequestMessage) -- ^ the stream the requestDispatcher reads from
  , sd_OwnPort         :: Port (ServerRequestMessage)   -- ^ the port the clients send messages to
  , sd_ClientMap       :: Map.Map IdType ClientInfo     -- ^ infomation of the the clients
  , sd_SiteToClientMap :: MMap.MultiMap SiteId IdType   -- ^ needed to get the closest client
  , sd_SiteMap         :: SiteMap                       -- ^ needed to get the closest site
  , sd_NextId          :: IdType
  }
  
-- | the server
data Server = Server (MVar ServerData)
  

-- | creates a new server
newServer 
  :: StreamName -> Maybe PortNumber -> IO Server
newServer sn pn
  = do
    -- create a new server
    st    <- (newStream STGlobal (Just sn) pn::IO (Stream ServerRequestMessage))
    po    <- newPortFromStream st
    tid   <- newMVar Nothing
    let sd = ServerData tid st po Map.empty MMap.empty Map.empty 1
    s <- newMVar sd
    let server =  Server s
    -- start the requestDispatcher to handle requests
    startRequestDispatcher tid st (dispatchServerRequest server)
    return server


-- | closes the server
closeServer :: Server -> IO ()
closeServer s@(Server server)
  = do
    allIds <- modifyMVar server $
      \sd ->
      do    
      -- shutdown the server thread
      stopRequestDispatcher (sd_ServerThreadId sd)
      -- close the stream
      closeStream (sd_OwnStream sd)
      -- getAll ClientIds
      let allIds = Map.keys (sd_ClientMap sd)
      return (sd, allIds)
    mapM (\i -> do unregisterClient i s) allIds
    return ()


-- | handles the requests from the client
dispatchServerRequest
  :: Server
  -> ServerRequestMessage
  -> Port (ServerResponseMessage)
  -> IO ()
dispatchServerRequest server msg replyPort
  = do
    case msg of
      (SReqRegisterClient s po) ->
        do
        handleRequest replyPort (registerClient s po server) (\i -> SRspRegisterClient i)
        return ()
      (SReqUnregisterClient n) ->
        do
        handleRequest replyPort (unregisterClient n server) (\_ -> SRspUnregisterClient)
        return ()
      (SReqPing i) ->
        do
        handleRequest replyPort (pingServer i server) (\b -> SRspPing b)
        return ()
      _ -> 
        handleRequest replyPort (return ()) (\_ -> SRspUnknown)


-- | creates a new client id and updates the serverdata
getNextId :: ServerData -> (IdType, ServerData)
getNextId sd 
  = (i, sd { sd_NextId = nid })
  where
    i   = sd_NextId sd
    nid = i + 1


-- | adds a new client to the server datastructures
--   the ping-thread will not be started
addClientToServer
  :: IdType -> SiteId -> ClientPort -> MVar (Maybe ThreadId)
  -> ServerData -> ServerData
addClientToServer i sid cp tid sd
  = sd { sd_ClientMap = nsm', sd_SiteToClientMap = snm', sd_SiteMap = sm' }
  where
    --update the ClientMap
    nsm = sd_ClientMap sd
    nsm' = Map.insert i (ClientInfo sid cp tid) nsm
    --update the SiteToClientMap
    snm = sd_SiteToClientMap sd
    snm' = MMap.insert sid i snm
    -- update the SiteMap
    sm = sd_SiteMap sd
    sm' = addIdToMap sid sm
    

-- | deletes a new client from the server datastructures
--   the ping-thread will not be closed 
deleteClientFromServer :: IdType -> ServerData -> ServerData
deleteClientFromServer i sd 
  = sd { sd_ClientMap = nsm', sd_SiteToClientMap = snm', sd_SiteMap = sm' }
  where
    --update the ClientMap
    nsm = sd_ClientMap sd
    nsm' = Map.delete i nsm
    --update the SiteToClientMap and the SiteIdMap
    info = lookupClientInfo i sd
    snm = sd_SiteToClientMap sd
    sm = sd_SiteMap sd
    (snm', sm') = deleteSiteId info
    deleteSiteId Nothing = (snm, sm)
    deleteSiteId (Just info') = (MMap.deleteElem sid i snm , deleteIdFromMap sid sm)
      where
      sid = ci_Site info'


-- gets the ClientPort from a ClientId
lookupClientInfo :: IdType -> ServerData -> Maybe ClientInfo
lookupClientInfo i sd = Map.lookup i (sd_ClientMap sd) 

instance ServerClass Server where
  registerClient sid po s@(Server server)
    = do
      let cp = newClientPort po
      -- register the client at the server
      (ptid,i) <- modifyMVar server $
        \sd ->
        do
        -- create a new Id and a new Port
        let (i, sd') = getNextId sd
        -- add node to controller
        ptid <- newMVar Nothing  
        let sd'' = addClientToServer i sid cp ptid sd'
        return (sd'', (ptid, i))
      -- startPingProcess for Client
      startThread ptid 5000000 (checkClient i cp ptid s)
      return i
     
  unregisterClient i (Server server)
    = do
      mbInfo <- modifyMVar server $
        \sd ->
        do
        let mbInfo = lookupClientInfo i sd 
        let sd' = deleteClientFromServer i sd
        return (sd', mbInfo)
      case mbInfo of
        (Just info) -> stopThread (ci_PingThreadId info)
        (Nothing)   -> return () 
        
  pingServer i (Server server)
    = withMVar server $
        \sd -> return $ isJust $ lookupClientInfo i sd
        


instance Debug Server where
  printDebug (Server server)
    = withMVar server $
        \sd ->
        do
        putStrLn "printServer"
        putStrLn $ "OwnStream:       " ++ show (sd_OwnStream sd)
        putStrLn $ "OwnPort:         " ++ show (sd_OwnPort sd)
        putStrLn $ "ClientMap:       " ++ show (sd_ClientMap sd)
        putStrLn $ "SiteToClientMap: " ++ show (sd_SiteToClientMap sd)
        putStrLn $ "SiteMap:         " ++ show (sd_SiteMap sd)
        putStrLn $ "NextId:          " ++ show (sd_NextId sd)
  


-- ----------------------------------------------------------------------------
-- Server-Port
-- ----------------------------------------------------------------------------

-- | the ServerPort is only a wrapper for a Port-Datatype  
data ServerPort = ServerPort (Port ServerRequestMessage)
  deriving (Show)


-- | creates a new ServerPort
newServerPort :: Port ServerRequestMessage -> ServerPort
newServerPort p = ServerPort p

-- | the instanciation of the 
instance ServerClass ServerPort where
  registerClient sid po (ServerPort p) 
    = do
      withStream $
        \s -> performPortAction p s time30 (SReqRegisterClient sid po) $
          \rsp ->
          do
          case rsp of
            (SRspRegisterClient n) -> return (Just n)
            _ -> return Nothing
  
  unregisterClient i (ServerPort p)
    = do
      withStream $
        \s -> performPortAction p s time30 (SReqUnregisterClient i) $
          \rsp ->
          do
          case rsp of
            (SRspUnregisterClient) -> return (Just ())
            _ -> return Nothing
  
  pingServer i (ServerPort p)
    = do
      withStream $
        \s -> performPortAction p s time30 (SReqPing i) $
          \rsp ->
          do
          case rsp of
            (SRspPing b) -> return (Just b)
            _ -> return Nothing 
  
  


-- ----------------------------------------------------------------------------
-- Client-Messages
-- ----------------------------------------------------------------------------
  
  -- | Requests datatype, which is send to a filesystem node.
data ClientRequestMessage
  = CReqPing IdType 
  | CReqUnknown         
  deriving (Show)

instance Binary ClientRequestMessage where
  put (CReqPing i)  = putWord8 1 >> put i 
  put (CReqUnknown) = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> get >>= \i -> return (CReqPing i)
        _ -> return (CReqUnknown)


-- | Response datatype from a filesystem node.
data ClientResponseMessage
  = CRspPing Bool
  | CRspError String
  | CRspUnknown
  deriving (Show)      

instance Binary ClientResponseMessage where
  put (CRspPing b)  = putWord8 1 >> put b
  put (CRspError e) = putWord8 2 >> put e
  put (CRspUnknown) = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> get >>= \b -> return (CRspPing b)
        2 -> get >>= \e -> return (CRspError e)
        _ -> return (CRspUnknown)

instance RspMsg ClientResponseMessage where
  isError (CRspError _) = True
  isError _ = False
  
  getErrorMsg (CRspError e) = e
  getErrorMsg _ = ""
  
  isUnknown (CRspUnknown) = True
  isUnknown _ = False
  
  mkErrorMsg e = CRspError e



-- ----------------------------------------------------------------------------
-- Client-TypeClass
-- ----------------------------------------------------------------------------


class ClientClass c where
  pingClient :: IdType-> c -> IO Bool
        


  
-- ----------------------------------------------------------------------------
-- Client-Data
-- ----------------------------------------------------------------------------

  
-- | client datatype.
data ClientData = ClientData {
    cd_ServerThreadId  :: MVar (Maybe ThreadId)
  , cd_PingThreadId    :: MVar (Maybe ThreadId)
  , cd_Id              :: Maybe IdType
  , cd_SiteId          :: SiteId
  , cd_OwnStream       :: Stream ClientRequestMessage
  , cd_OwnPort         :: Port ClientRequestMessage
  , cd_ServerPort      :: ServerPort
  }
  
-- | only a wrapper around an MVar
data Client = Client (MVar ClientData)


-- | creates a new client, it needs the StreamName and optional the SocketId of the server
newClient :: StreamName -> Maybe SocketId -> IO Client
newClient sn soid
  = do
    p <- newPort sn soid
    let sp = (newServerPort p)
    -- initialize values
    sid     <- getSiteId
    stid    <- newMVar Nothing
    ptid    <- newMVar Nothing
    st      <- (newLocalStream Nothing::IO (Stream ClientRequestMessage))
    po      <- newPortFromStream st
    let cd = (ClientData stid ptid Nothing sid st po sp)
    c <- newMVar cd
    let client = Client c 
    -- first, we start the server, because we can't handle requests without it
    startRequestDispatcher stid st (dispatchClientRequest client)
    -- then we try to register a the server
    startThread ptid 5000000 (checkServer sp sid po client)
    return client


-- | closes the client
closeClient :: Client -> IO ()
closeClient (Client client)
  = modifyMVar client $
      \cd ->
      do
      case (cd_Id cd) of
        (Just i)  -> unregisterClient i (cd_ServerPort cd)
        (Nothing) -> return ()
      stopRequestDispatcher (cd_ServerThreadId cd)
      closeStream (cd_OwnStream cd)
      stopThread (cd_PingThreadId cd)
      return (cd, ())


-- | handles the requests from the server
dispatchClientRequest
  :: Client
  -> ClientRequestMessage 
  -> Port ClientResponseMessage
  -> IO ()
dispatchClientRequest client msg replyPort
  = do
    case msg of
      (CReqPing i) ->
        do
        handleRequest replyPort (pingClient i client) (\b -> CRspPing b)
        return ()
      _ -> 
        handleRequest replyPort (return ()) (\_ -> CRspUnknown)



instance ClientClass Client where
  pingClient i (Client client)
    = withMVar client $
        \cd -> return $ (cd_Id cd) == (Just i)


instance Debug Client where
  printDebug (Client client)
    = withMVar client $
        \cd ->
        do
        putStrLn "printClient"
        putStrLn $ "Id:         " ++ show (cd_Id cd)
        putStrLn $ "Site:       " ++ show (cd_SiteId cd)
        putStrLn $ "OwnStream:  " ++ show (cd_OwnStream cd)
        putStrLn $ "OwnPort:    " ++ show (cd_OwnPort cd)
        putStrLn $ "ServerPort: " ++ show (cd_ServerPort cd)




-- ----------------------------------------------------------------------------
-- Client-Port
-- ----------------------------------------------------------------------------

-- | just a wrapper around a port
data ClientPort = ClientPort (Port ClientRequestMessage)
  deriving (Show)


-- | creates a new ClientPort
newClientPort :: Port ClientRequestMessage -> ClientPort
newClientPort po = ClientPort po



instance ClientClass ClientPort where
  pingClient i (ClientPort p)
    = do
      withStream $
        \s -> performPortAction p s time30 (CReqPing i) $
          \rsp ->
          do
          case rsp of
            (CRspPing b) -> return (Just b)
            _ -> return Nothing



-- ----------------------------------------------------------------------------
-- Ping-Functions
-- ----------------------------------------------------------------------------

-- | checks, if a client is still reachable, otherwise it will be deleted from the server
checkClient :: IdType -> ClientPort -> MVar (Maybe ThreadId) -> Server -> IO ()
checkClient i po mVarTid server
  = E.handle (\e -> 
      do
      debugM localLogger $ show e
      deleteClient
     ) $
      do
      debugM localLogger "pingClient"
      b <- pingClient i po
      case b of
        False -> 
          do
          debugM localLogger "pingCient: client is not reachable... delete him"
          deleteClient
        True  -> 
          do
          debugM localLogger "pingCient: client is ok"
          return ()
    where
     deleteClient
       = do
         unregisterClient i server
         forkIO $ stopThread mVarTid
         return ()     


-- | checks, if a server is still reachable, otherwise it will be deleted from the client
checkServer :: ServerPort -> SiteId -> Port ClientRequestMessage -> Client -> IO ()
checkServer sepo sid clpo c
  = E.handle
     (\e -> 
      do
      debugM localLogger $ show e
      deleteServer c
     ) $
      do
      debugM localLogger "pingServer"
      (reg,i) <- isClientRegistered c
      if (reg)
        then do
          debugM localLogger "pingServer: client is registered, testing server" 
          b <- pingServer (fromJust i) sepo
          if (b)
            then do
              debugM localLogger "pingServer: server is ok"  
              return ()              
            else do
              debugM localLogger "pingServer: server is down" 
              deleteServer c
        else do 
          debugM localLogger "pingServer: trying to register client"
          i' <- registerClient sid clpo sepo
          addServer i' c
    where
    isClientRegistered (Client client)
      = withMVar client $ \cd -> return $ (isJust (cd_Id cd), (cd_Id cd))
    deleteServer (Client client)
      = modifyMVar client $ \cd -> return ( cd {cd_Id = Nothing}, ())
    addServer i (Client client)
      = modifyMVar client $ \cd -> return ( cd {cd_Id = Just i}, ())  
