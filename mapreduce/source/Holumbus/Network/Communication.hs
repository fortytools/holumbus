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
  StreamName
, SocketId
, PortNumber
-- time constants
, time30

, IdType

, ClientInfo(..)

-- * server operations
, Server
, newServer
, closeServer
, ServerPort
, newServerPort
, sendRequestToServer

, getClientInfo
, getAllClientInfos

-- * client operations
, ClientClass(..)
, Client
, newClient
, closeClient
, ClientPort
, sendRequestToClient 
)
where

import           Control.Concurrent
import           Data.Binary
import qualified Data.ByteString.Lazy as B
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
  | SReqServerAction B.ByteString
  | SReqUnknown
  deriving (Show)

instance Binary (ServerRequestMessage) where
  put (SReqRegisterClient sid po) = putWord8 1 >> put sid >> put po
  put (SReqUnregisterClient i)    = putWord8 2 >> put i
  put (SReqPing i)                = putWord8 3 >> put i
  put (SReqServerAction b)        = putWord8 4 >> put b
  put (SReqUnknown)               = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> get >>= \sid -> get >>= \po -> return (SReqRegisterClient sid po)
        2 -> get >>= \i -> return (SReqUnregisterClient i)
        3 -> get >>= \i -> return (SReqPing i)
        4 -> get >>= \b -> return (SReqServerAction b)
        _ -> return (SReqUnknown)


-- | the responses the server gives
data ServerResponseMessage
  = SRspSuccess
  | SRspRegisterClient IdType
  | SRspUnregisterClient
  | SRspPing Bool
  | SRspServerAction B.ByteString
  | SRspError String
  | SRspUnknown
  deriving (Show)

instance Binary (ServerResponseMessage) where
  put (SRspSuccess)          = putWord8 1
  put (SRspRegisterClient i) = putWord8 2 >> put i
  put (SRspUnregisterClient) = putWord8 3
  put (SRspPing b)           = putWord8 4 >> put b
  put (SRspServerAction b)   = putWord8 5 >> put b
  put (SRspError e)          = putWord8 6 >> put e
  put (SRspUnknown)          = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> return (SRspSuccess)
        2 -> get >>= \i -> return (SRspRegisterClient i)
        3 -> return (SRspUnregisterClient)
        4 -> get >>= \b -> return (SRspPing b)
        5 -> get >>= \b -> return (SRspServerAction b)
        6 -> get >>= \e -> return (SRspError e)
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
  
type RegistrationAction = (IdType -> ClientPort -> IO ())
  
-- the information of the client known by the server
data ClientInfo = ClientInfo {
    ci_Site         :: SiteId                -- ^ SiteId (Hostname,PID) of the client process
  , ci_Port         :: ClientPort            -- ^ the port of the client
  , ci_PingThreadId :: Thread                -- ^ the threadId of the ping-Process (needed to stop it)
  }
  
instance Show ClientInfo where
  show (ClientInfo s p _) = "{Site: " ++ show s ++ " - Port: " ++ show p ++ "}"


-- | the data of the server needed to organise the clients
data ServerData = ServerData {
    sd_ServerThreadId  :: Thread                        -- ^ threadId of the streamDispatcher
  , sd_OwnStream       :: Stream (ServerRequestMessage) -- ^ the stream the requestDispatcher reads from
  , sd_OwnPort         :: Port (ServerRequestMessage)   -- ^ the port the clients send messages to
  , sd_ClientMap       :: Map.Map IdType ClientInfo     -- ^ infomation of the the clients
  , sd_SiteToClientMap :: MMap.MultiMap SiteId IdType   -- ^ needed to get the closest client
  , sd_SiteMap         :: SiteMap                       -- ^ needed to get the closest site
  , sd_Register        :: RegistrationAction
  , sd_Unregister      :: RegistrationAction
  , sd_NextId          :: IdType
  }
  
-- | the server
data Server = Server (MVar ServerData)
  

-- | creates a new server
newServer
  :: (Binary a, Binary b)
  => StreamName -> Maybe PortNumber
  -> (a -> IO (Maybe b))             -- ^ handling own request
  -> Maybe RegistrationAction        -- ^ for registration
  -> Maybe RegistrationAction        -- ^ for unregistration 
  -> IO Server
newServer sn pn dispatch register unregister
  = do
    -- create a new server
    st    <- (newStream STGlobal (Just sn) pn::IO (Stream ServerRequestMessage))
    po    <- newPortFromStream st
    tid   <- newThread
    let reg   = maybe (\_ _ -> return ()) id register
    let unreg = maybe (\_ _ -> return ()) id unregister
    let sd = ServerData tid st po Map.empty MMap.empty Map.empty reg unreg 1
    s <- newMVar sd
    let server =  Server s
    -- start the requestDispatcher to handle requests
    startRequestDispatcher tid st (dispatchServerRequest server dispatch)
    return server


-- | closes the server
closeServer :: Server -> IO ()
closeServer s@(Server server)
  = do
    debugM localLogger "closeServer: start"
    (allIds,thread,stream) <- withMVar server $
      \sd ->
      do
      -- getAll ClientIds
      let allIds = Map.keys (sd_ClientMap sd)
      return (allIds, sd_ServerThreadId sd, sd_OwnStream sd)          
    debugM localLogger "closeServer: stopRequestDispatcher"
    -- shutdown the server thread
    stopRequestDispatcher thread
    -- close the stream
    debugM localLogger "closeServer: closeStream"
    closeStream stream
    debugM localLogger "closeServer: unregister clients"
    mapM (\i -> do unregisterClient i s) allIds
    debugM localLogger "closeServer: end"
    return ()


-- | handles the requests from the client
dispatchServerRequest
  :: (Binary a, Binary b)
  => Server
  -> (a -> IO (Maybe b))
  -> ServerRequestMessage
  -> Port (ServerResponseMessage)
  -> IO ()
dispatchServerRequest server action msg replyPort
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
      (SReqServerAction b) ->
        do
        handleRequest replyPort
          (action $ decode b) 
          (\res -> maybe (SRspUnknown) (\r -> SRspServerAction $ encode r) res)
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
  :: IdType -> SiteId -> ClientPort -> Thread
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


lookupAllClientInfos :: ServerData -> [ClientInfo]
lookupAllClientInfos sd = Map.elems (sd_ClientMap sd)


getClientInfo :: IdType -> Server -> IO (Maybe ClientInfo)
getClientInfo i (Server server)
  = withMVar server $ \sd -> return $ lookupClientInfo i sd 


getAllClientInfos :: Server -> IO [ClientInfo]
getAllClientInfos (Server server)
  = withMVar server $ \sd -> return $ lookupAllClientInfos sd


instance ServerClass Server where
  registerClient sid po s@(Server server)
    = do
      let cp = newClientPort po
      -- register the client at the server
      (ptid,i,register) <- modifyMVar server $
        \sd ->
        do
        -- create a new Id and a new Port
        let (i, sd') = getNextId sd
        let register = sd_Register sd
        -- add node to controller
        ptid <- newThread
        let sd'' = addClientToServer i sid cp ptid sd'
        return (sd'', (ptid, i, register))
      -- do general registration action
      register i cp
      -- startPingProcess for Client
      setThreadDelay 5000000 ptid
      setThreadAction (checkClient i cp s ptid) ptid
      setThreadErrorHandler (handleCheckClientError i s ptid) ptid
      startThread ptid
      return i
     
  unregisterClient i (Server server)
    = do
      debugM localLogger "unregisterClient: start"
      (mbInfo, unregister) <- modifyMVar server $
        \sd ->
        do
        let unregister = sd_Unregister sd
        let mbInfo = lookupClientInfo i sd 
        let sd' = deleteClientFromServer i sd
        return (sd', (mbInfo,unregister))
      debugM localLogger "unregisterClient: client deleted"
      case mbInfo of
        (Just info) -> 
          do
          debugM localLogger "unregisterClient: killing the ping thread"
          -- kill the ping-thread
          stopThread (ci_PingThreadId info)
          debugM localLogger "unregisterClient: ping thread killed"
          debugM localLogger "unregisterClient: executing unregister-function"
          -- execute the unregister function
          unregister i (ci_Port info)
          debugM localLogger "unregisterClient: unregister-function executed"
        (Nothing)   ->
          do
          debugM localLogger "unregisterClient: no client info found"
          return ()
      debugM localLogger "unregisterClient: end"
        
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

instance Binary ServerPort where
  put (ServerPort p) = put p
  get
    = do
      p <- get
      return (ServerPort p)
      

-- | creates a new ServerPort
-- newServerPort :: Port ServerRequestMessage -> ServerPort
-- newServerPort p = ServerPort p

newServerPort :: StreamName -> Maybe SocketId -> IO ServerPort
newServerPort sn soid
  = do
    p <- newPort sn soid
    return (ServerPort p)

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
  | CReqClientAction B.ByteString
  | CReqClientId
  | CReqServerPort
  | CReqUnknown         
  deriving (Show)

instance Binary ClientRequestMessage where
  put (CReqPing i)         = putWord8 1 >> put i
  put (CReqClientAction b) = putWord8 2 >> put b
  put (CReqClientId)       = putWord8 3
  put (CReqServerPort)     = putWord8 4
  put (CReqUnknown)        = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> get >>= \i -> return (CReqPing i)
        2 -> get >>= \b -> return (CReqClientAction b)
        3 -> return (CReqClientId)
        4 -> return (CReqServerPort)
        _ -> return (CReqUnknown)


-- | Response datatype from a filesystem node.
data ClientResponseMessage
  = CRspPing Bool
  | CRspClientAction B.ByteString
  | CRspClientId (Maybe IdType)
  | CRspServerPort ServerPort
  | CRspError String
  | CRspUnknown
  deriving (Show)      

instance Binary ClientResponseMessage where
  put (CRspPing b)         = putWord8 1 >> put b
  put (CRspClientAction b) = putWord8 2 >> put b
  put (CRspClientId i)     = putWord8 3 >> put i
  put (CRspServerPort p)   = putWord8 4 >> put p
  put (CRspError e)        = putWord8 5 >> put e
  put (CRspUnknown)        = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> get >>= \b -> return (CRspPing b)
        2 -> get >>= \b -> return (CRspClientAction b)
        3 -> get >>= \i -> return (CRspClientId i)
        4 -> get >>= \p -> return (CRspServerPort p)
        5 -> get >>= \e -> return (CRspError e)
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
  getClientId :: c -> IO (Maybe IdType)
  getServerPort :: c -> IO (ServerPort)
        


  
-- ----------------------------------------------------------------------------
-- Client-Data
-- ----------------------------------------------------------------------------

  
-- | client datatype.
data ClientData = ClientData {
    cd_ServerThreadId  :: Thread
  , cd_PingThreadId    :: Thread
  , cd_Id              :: Maybe IdType
  , cd_SiteId          :: SiteId
  , cd_OwnStream       :: Stream ClientRequestMessage
  , cd_OwnPort         :: Port ClientRequestMessage
  , cd_ServerPort      :: ServerPort
  }
  
-- | only a wrapper around an MVar
data Client = Client (MVar ClientData)


-- | creates a new client, it needs the StreamName and optional the SocketId of the server
newClient
  :: (Binary a, Binary b)
  => StreamName -> Maybe SocketId
  -> (a -> IO (Maybe b))  -- | the individual request dispatcher for the client
  -> IO Client
newClient sn soid action
  = do  
    sp <- newServerPort sn soid
    -- initialize values
    sid     <- getSiteId
    stid    <- newThread  
    st      <- (newLocalStream Nothing::IO (Stream ClientRequestMessage))
    po      <- newPortFromStream st
    ptid    <- newThread
    let cd = (ClientData stid ptid Nothing sid st po sp)
    c <- newMVar cd
    let client = Client c 
    -- first, we start the server, because we can't handle requests without it
    startRequestDispatcher stid st (dispatchClientRequest client action)
    -- then we try to register a the server
    setThreadDelay 5000000 ptid
    setThreadAction (checkServer sp sid po client) ptid
    setThreadErrorHandler (handleCheckServerError client) ptid
    startThread ptid
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
  :: (Binary a, Binary b)
  => Client
  -> (a -> IO (Maybe b))
  -> ClientRequestMessage 
  -> Port ClientResponseMessage
  -> IO ()
dispatchClientRequest client action msg replyPort
  = do
    case msg of
      (CReqPing i) ->
        do
        handleRequest replyPort (pingClient i client) (\b -> CRspPing b)
        return ()
      (CReqClientAction b) ->
        do
        -- now, we have a specific client request
        handleRequest replyPort 
          (action $ decode b) 
          (\res -> maybe (CRspUnknown) (\r -> CRspClientAction $ encode r) res)
      _ -> 
        handleRequest replyPort (return ()) (\_ -> CRspUnknown)


isClientRegistered :: Client -> IO (Bool, Maybe IdType)
isClientRegistered (Client client)
  = withMVar client $ \cd -> return $ (isJust (cd_Id cd), (cd_Id cd))


unsetClientId :: Client -> IO ()
unsetClientId (Client client)
  = modifyMVar client $ \cd -> return ( cd {cd_Id = Nothing}, ())


setClientId :: IdType -> Client -> IO ()
setClientId i (Client client)
      = modifyMVar client $ \cd -> return ( cd {cd_Id = Just i}, ())  


instance ClientClass Client where
  pingClient i (Client client)
    = withMVar client $
        \cd -> return $ (cd_Id cd) == (Just i)
        
  getClientId (Client client)
    = withMVar client $
        \cd -> return $ (cd_Id cd)
    
  getServerPort (Client client)
    = withMVar client $
        \cd -> return $ (cd_ServerPort cd)


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

instance Binary ClientPort where
  put (ClientPort p) = put p
  get
    = do
      p <- get
      return (ClientPort p)


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

  getClientId (ClientPort p)
    = do
      withStream $
        \s -> performPortAction p s time30 (CReqClientId) $
          \rsp ->
          do
          case rsp of
            (CRspClientId i) -> return (Just i)
            _ -> return Nothing
    
  getServerPort (ClientPort p)
    = do
      withStream $
        \s -> performPortAction p s time30 (CReqServerPort) $
          \rsp ->
          do
          case rsp of
            (CRspServerPort sp) -> return (Just sp)
            _ -> return Nothing



sendRequestToClient 
  :: (Binary a, Binary b)
  => ClientPort -> Int
  -> a
  -> (b -> IO (Maybe c))  -- ^ response handler
  -> IO c
sendRequestToClient (ClientPort p) timeout a handler
  = do
    withStream $
      \s -> performPortAction p s timeout (CReqClientAction (encode a)) $
        \rsp ->
        do
        case rsp of
          (CRspClientAction b) -> 
            do
            handler (decode b)
          _ -> return Nothing


sendRequestToServer 
  :: (Binary a, Binary b)
  => ServerPort -> Int
  -> a
  -> (b -> IO (Maybe c))  -- ^ response handler
  -> IO c
sendRequestToServer (ServerPort p) timeout a handler
  = do
    withStream $
      \s -> performPortAction p s timeout (SReqServerAction (encode a)) $
        \rsp ->
        do
        case rsp of
          (SRspServerAction b) -> 
            do
            handler (decode b)
          _ -> return Nothing

          
-- ----------------------------------------------------------------------------
-- Ping-Functions
-- ----------------------------------------------------------------------------

-- | checks, if a client is still reachable, otherwise it will be deleted from the server
checkClient :: IdType -> ClientPort -> Server -> Thread -> IO ()
checkClient i po server thread
  = do
    debugM localLogger "pingClient"
    b <- pingClient i po
    case b of
      False -> 
        do
        warningM localLogger "pingCient: client is not reachable... delete him"
        handleCheckClientError i server thread
      True  -> 
        do
        debugM localLogger "pingCient: client is ok"
        return ()
    
    
handleCheckClientError :: IdType -> Server -> Thread -> IO ()
handleCheckClientError i server thread
   = do
     unregisterClient i server
     stopThread thread
     return ()     


-- | checks, if a server is still reachable, otherwise it will be deleted from the client
checkServer :: ServerPort -> SiteId -> Port ClientRequestMessage -> Client -> IO ()
checkServer sepo sid clpo c
  = do
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
            warningM localLogger "pingServer: server is down" 
            handleCheckServerError c
      else do 
        debugM localLogger "pingServer: trying to register client"
        i' <- registerClient sid clpo sepo
        setClientId i' c    


handleCheckServerError :: Client -> IO ()
handleCheckServerError c
  = do
    unsetClientId c