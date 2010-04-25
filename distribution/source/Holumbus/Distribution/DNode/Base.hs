-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Distribution.DNode.Base
  Copyright  : Copyright (C) 2009 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
-}

-- ----------------------------------------------------------------------------

{-# OPTIONS_GHC -XDeriveDataTypeable -XScopedTypeVariables #-}
module Holumbus.Distribution.DNode.Base
(
    DistributedException(..)
    
  , DNodeConfig(..)
  , defaultDNodeConfig
  
  , DNodeId
  , mkDNodeId
  , DNodeAddress
  , mkDNodeAddress
  
  , DHandlerId
  
  -- rf == only to be used by new resource implementations

  , DRessourceType                -- rf
  , mkDRessourceType              -- rf
  
  , DRessourceId
  , DRessourceAddress
  , mkDRessourceAddress           -- rf
  
  , DRessourceDispatcher          -- rf
  , DRessourceEntry(..)           -- rf
    
  , initDNode
  , deinitDNode
  , addForeignDNode
  , delForeignDNode
  , checkForeignDNode
  , addForeignDNodeHandler
  , addForeignDRessourceHandler
  , delForeignHandler
  
  , genLocalRessourceAddress      -- rf
  , addLocalRessource             -- rf
  , delLocalRessource             -- rf
  , delForeignRessource           -- rf
  , safeAccessForeignRessource    -- rf
  , unsafeAccessForeignRessource  -- rf
  , getByteStringMessage          -- rf -- reimported from Network-Module
  , putByteStringMessage          -- rf -- reimported from Network-Module
  , getDNodeData                  -- debug
)
where

import           Prelude hiding (catch)

import           Control.Exception
import           Control.Concurrent
import           Data.Typeable
import           Data.Binary
import           Data.Char
import           Data.Maybe
import           Data.Unique
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Network.Socket (HostName, PortNumber)
import           System.IO
import           System.IO.Unsafe
import           System.Log.Logger
import           System.Random

import           Holumbus.Distribution.DNode.Network


localLogger :: String
localLogger = "Holumbus.Distribution.DNode.Base"


-- ----------------------------------------------------------------------------
-- Basic types which are commonly used
-- ----------------------------------------------------------------------------

-- | The exception type, used by distributed communication
data DistributedException = DistributedException {
    distEx_msg :: String
  , distEx_fct :: String
  , distEx_mod :: String
  } deriving(Typeable, Show)
instance Exception DistributedException


-- | A distributed Id type, could be named or randomly generated
data DId
  = DIdName String
  | DIdNumber Integer
  deriving (Show, Eq, Ord)
  
instance Binary DId where
  put (DIdName s) = putWord8 0 >> put s
  put (DIdNumber n) = putWord8 1 >> put n
  get
    = do
      t <- getWord8
      case t of
        0 -> get >>= \s -> return (DIdName s)
        _ -> get >>= \n -> return (DIdNumber n)
  
-- | Generates a distributed Id value. If the string is empty, a random
--   but unique id will be created
genDId :: String -> IO DId
genDId "" 
  = do
    -- TODO make more unique -> uuids
    i <- getStdRandom (randomR (1,1000000)):: IO Integer
    return $ DIdNumber i
genDId st
  = do
    return $ DIdName st
  


-- ----------------------------------------------------------------------------
-- DNode related types
-- ----------------------------------------------------------------------------

-- | The configuration of a DNode. You need it to create a DNode and you can 
--   use this data type it to alter its properties. This type is public to
--   allow users to create their own configuration.
data DNodeConfig = DNodeConfig {
    dnc_Name        :: String
  , dnc_MinPort     :: Int
  , dnc_MaxPort     :: Int
  , dnc_AccessDelay :: Int
  , dnc_PingDelay   :: Int
  } deriving (Show)

-- | A good default configuration. To create an unnamed node, just leave the
--   string empty. 
defaultDNodeConfig :: String -> DNodeConfig
defaultDNodeConfig s
  = DNodeConfig s 8000 9000 1000000 1000000



-- | The DNode identifier.
--   Every DNode has an Id, this could be named or randomly created. The id 
--   could not be used to address a DNode directly over a Network connection 
--   because the physical references are missing. The DNodeId is meant to 
--   create a declarative reference which could be used to lookup purposes. 
--   Think of the DNodeId as a domain name, without a DNS-Server to resolve 
--   the physical address, it is worthless to establish a communication. 
newtype DNodeId = DNodeId DId
  deriving (Show, Eq, Ord)

instance Binary DNodeId where
  put (DNodeId i) = put i
  get = get >>= \i -> return (DNodeId i)
  
-- | Generates a new DNodeId. If the string is empty, a random but unique
--   id will be created. 
genDNodeId :: String -> IO DNodeId
genDNodeId s = genDId s >>= \i -> return (DNodeId i)

-- | Use this to make a new DNodeId from a String
mkDNodeId :: String -> DNodeId
mkDNodeId "" = error "the name of DNode is empty"
mkDNodeId s  = DNodeId $ DIdName s


-- | The DNode address.
data DNodeAddress = DNodeAddress {
    dna_Id          :: DNodeId
  , dna_HostName    :: HostName
  , dna_ServiceName :: PortNumber
  } deriving (Show, Eq, Ord)

instance Binary DNodeAddress where
  put (DNodeAddress i hn po) = put i >> put hn >> put (toInteger po)
  get = get >>= \i -> get >>= \hn -> get >>= \po -> return (DNodeAddress i hn (fromInteger po))


-- | use this to make a new DNodeAddress
mkDNodeAddress :: String -> HostName -> Int -> DNodeAddress
mkDNodeAddress s hn po = DNodeAddress i hn (fromIntegral po)
  where
  i = mkDNodeId s
  
type DHandlerFunction = DHandlerId -> IO ()

data DNodeEntry = DNodeEntry {
    dne_Address              :: DNodeAddress
  , dne_PingThreadId         :: Maybe ThreadId
  , dne_HandlerFunctions     :: Map.Map DHandlerId DHandlerFunction
  , dne_TriggeredHandlers    :: Set.Set DHandlerId
  , dne_PositiveNodeHandlers :: Set.Set DHandlerId
  , dne_NegativeNodeHandlers :: Set.Set DHandlerId
  , dne_PositiveResourceHandlers :: Set.Set DHandlerId
  , dne_NegativeResourceHandlers :: Set.Set DHandlerId
  }

instance Show DNodeEntry where
  show _ = "{DNodeEntry}"

data DHandlerId = DHandlerId {
    dhi_Id      :: Unique
  , dhi_DNodeId :: DNodeId
  , dhi_DRessourceId :: Maybe DRessourceId
  } deriving (Eq, Ord)

  
-- ----------------------------------------------------------------------------
-- DRessource related types
-- ----------------------------------------------------------------------------

-- | The ressouce type, it is used to separate between different kinds of
--   ressources. The ressource type is generated by the programmer of a
--   ressource
newtype DRessourceType = DRessourceType String
  deriving (Show, Eq, Ord)

instance Binary DRessourceType where
  put (DRessourceType s) = put s
  get = get >>= \s -> return (DRessourceType s)

mkDRessourceType :: String -> DRessourceType
mkDRessourceType s = DRessourceType (map toUpper s)



-- | The DRessource Id.
data DRessourceId = DRessourceId DId DRessourceType
  deriving (Show, Eq, Ord)

instance Binary DRessourceId where
  put (DRessourceId i t) = put i >> put t 
  get = get >>= \i -> get >>= \t -> return (DRessourceId i t)

genDRessourceId :: DRessourceType -> String -> IO DRessourceId
genDRessourceId t s = genDId s >>= \i -> return (DRessourceId i t)

mkDRessourceId :: DRessourceType -> String -> DRessourceId
mkDRessourceId _ "" = error "the name of the DRessource is empty"
mkDRessourceId t s  = DRessourceId (DIdName s) t


-- | The DRessource address
data DRessourceAddress = DRessourceAddress {
    dra_Id     :: DRessourceId
  , dra_NodeId :: DNodeId
  } deriving (Show, Eq, Ord)

instance Binary DRessourceAddress where
  put (DRessourceAddress i a) = put i >> put a
  get = get >>= \i -> get >>= \a -> return (DRessourceAddress i a)

mkDRessourceAddress :: DRessourceType -> String -> String -> DRessourceAddress
mkDRessourceAddress t r n = DRessourceAddress dri dni
  where
  dri = mkDRessourceId t r
  dni = mkDNodeId n

-- The DRessource callback functions
type DRessourceDispatcher   = DNodeId -> Handle -> IO ()

-- The container for the DRessources
data DRessourceEntry = DRessourceEntry {
    dre_Dispatcher   :: DRessourceDispatcher
  }

instance Show DRessourceEntry where
  show _ = "{DRessourceEntry}"



-- | The local DNode, 
--   manages the Ressources,
--   keeps a record of foreign DNodes
data DNodeData = DNodeData {
    dnd_Address         :: DNodeAddress
  , dnd_SocketServer    :: SocketServer
  , dnd_AccessDelay     :: Int
  , dnd_PingDelay       :: Int
  , dnd_NodeMap         :: Map.Map DNodeId DNodeEntry
  , dnd_OwnRessourceMap :: Map.Map DRessourceId DRessourceEntry
  } deriving (Show)

type DNode = MVar DNodeData


{-# NOINLINE myDNode #-}
myDNode :: DNode
myDNode
  = do
    unsafePerformIO $ newEmptyMVar


-- | Initializes the DNode of the program. You have to call this function
--   once BEFORE you can use other functions. 
initDNode :: DNodeConfig -> IO DNodeId
initDNode c
  = do
    let minPort     = fromIntegral $ dnc_MinPort c
        maxPort     = fromIntegral $ dnc_MaxPort c
        accessDelay = dnc_AccessDelay c
        pingDelay   = dnc_PingDelay c
    n   <- genDNodeId (dnc_Name c)
    serverSocket <- startSocketServer (dispatcher) minPort maxPort
    case serverSocket of
      Nothing ->
        do
        errorM localLogger "no socket opened"
        error "socket could not be opened"
      (Just ss) ->
        do
        let hn = getSocketServerName ss
            po = getSocketServerPort ss
            dnd = DNodeData {
            dnd_Address         = DNodeAddress n hn po
          , dnd_SocketServer    = ss
          , dnd_AccessDelay     = accessDelay
          , dnd_PingDelay       = pingDelay
          , dnd_NodeMap         = Map.empty
          , dnd_OwnRessourceMap = Map.empty
          }
        success <- tryPutMVar myDNode dnd
        if success
          then do return ()          
          else do
            stopSocketServer ss
            error "dnode already initialized"
    return n


-- | deinitializes a DNode
deinitDNode :: IO ()
deinitDNode
  = do
    mbDnd <- tryTakeMVar myDNode
    case mbDnd of
      (Just dnd) ->
        do
        -- TODO close all threads and unregister from all other nodes...
        -- close the dispatcher thread, so no requests are handled...
        stopSocketServer (dnd_SocketServer dnd)
      (Nothing) ->
        do
        error "dnode already deinitialized"
    
    

addNode :: DNodeAddress -> IO ()
addNode dna
  = do
    let dne = DNodeEntry dna Nothing Map.empty Set.empty Set.empty Set.empty Set.empty Set.empty
        dni = dna_Id dna
    modifyMVar_ myDNode $ \dnd -> return $ addNodeP dni dne dnd
   
      
addNodeP :: DNodeId -> DNodeEntry -> DNodeData -> DNodeData
addNodeP dni dne dnd = dnd { dnd_NodeMap = am }
  where
  am = Map.insert dni dne (dnd_NodeMap dnd)

deleteNode :: DNodeId -> IO ()
deleteNode i = modifyMVar_ myDNode $ \dnd -> return $ deleteNodeP i dnd 

deleteNodeP :: DNodeId -> DNodeData -> DNodeData
deleteNodeP i dnd = dnd { dnd_NodeMap = am }
  where
  am = Map.delete i (dnd_NodeMap dnd)

lookupNode :: DNodeId -> IO (Maybe DNodeEntry)
lookupNode i = withMVar myDNode $ \dnd -> return $ lookupNodeP i dnd

lookupNodeP :: DNodeId -> DNodeData -> Maybe DNodeEntry
lookupNodeP i dnd = Map.lookup i (dnd_NodeMap dnd)

-- lookupAllNodes :: IO ([DNodeEntry])
-- lookupAllNodes = withMVar myDNode $ \dnd -> return $ lookupAllNodesP dnd

-- lookupAllNodesP :: DNodeData -> [DNodeEntry]
-- lookupAllNodesP dnd = Map.elems (dnd_NodeMap dnd)

addRessource :: DRessourceId -> DRessourceEntry -> IO ()
addRessource i d = modifyMVar_ myDNode $ \dnd -> return $ addRessourceP dnd i d

addRessourceP :: DNodeData -> DRessourceId -> DRessourceEntry -> DNodeData
addRessourceP dnd i d = dnd { dnd_OwnRessourceMap = rm }
  where
  rm = Map.insert i d (dnd_OwnRessourceMap dnd)

deleteRessource :: DRessourceId -> IO ()
deleteRessource a = modifyMVar_ myDNode $ \dnd -> return $ deleteRessourceP dnd a

deleteRessourceP :: DNodeData -> DRessourceId -> DNodeData
deleteRessourceP dnd i = dnd { dnd_OwnRessourceMap = rm }
  where
  rm = Map.delete i (dnd_OwnRessourceMap dnd)

lookupRessource :: DRessourceId -> IO (Maybe DRessourceEntry)
lookupRessource i = withMVar myDNode $ \dnd -> return $ lookupRessourceP dnd i

lookupRessourceP :: DNodeData -> DRessourceId -> Maybe DRessourceEntry
lookupRessourceP dnd i = Map.lookup i (dnd_OwnRessourceMap dnd)


getDNodeData :: IO (DNodeData)
getDNodeData = readMVar myDNode


lookupDNodeReqestInfo :: DNodeId -> IO (DNodeAddress, Maybe DNodeAddress)
lookupDNodeReqestInfo dni
  = withMVar myDNode $ \dnd ->
      do
      let myDna      = dnd_Address dnd
          mbDne      = lookupNodeP dni dnd
          mbOtherDna = if (isJust mbDne) then (Just $ dne_Address $ fromJust mbDne) else Nothing 
      return (myDna, mbOtherDna)


checkDNodeRequest :: DNodeAddress -> DNodeAddress -> IO Bool
checkDNodeRequest s r
  = modifyMVar myDNode $ \dnd ->
      do
      let isValid = (dna_Id r) == (dna_Id $ dnd_Address dnd)
          dni     = dna_Id s
          dne     = DNodeEntry s Nothing Map.empty Set.empty Set.empty Set.empty Set.empty Set.empty
          dnd'    = if (Map.member dni (dnd_NodeMap dnd))
                    then dnd 
                    else addNodeP dni dne dnd
      return (dnd', isValid)    

-- ----------------------------------------------------------------------------

-- generic message container
data DNodeRequestMessage = DNodeRequestMessage {
    dnm_Req_Sender   :: DNodeAddress
  , dnm_Req_Receiver :: DNodeAddress
  , dnm_Req_Message  :: DNodeRequest
  } deriving (Show)
  
instance Binary DNodeRequestMessage where
  put (DNodeRequestMessage s r m) = put s >> put r >> put m
  get = get >>= \s -> get >>= \r -> get >>= \m -> return (DNodeRequestMessage s r m)

data DNodeRequest
  = DNMReqPing DNodeId [DRessourceId]
  | DNMReqRessourceMsg DRessourceAddress
  deriving (Show)

instance Binary DNodeRequest where
  -- put (DNMReqRegister dna)   = putWord8 1 >> put dna
  -- put (DNMReqUnregister i)   = putWord8 2 >> put i
  put (DNMReqPing i rs)      = putWord8 3 >> put i >> put rs
  put (DNMReqRessourceMsg a) = putWord8 4 >> put a
  get
    = do
      t <- getWord8
      case t of
        -- 1 -> get >>= \dna -> return (DNMReqRegister dna)
        -- 2 -> get >>= \i -> return (DNMReqUnregister i)
        3 -> get >>= \i -> get >>= \rs -> return (DNMReqPing i rs)
        4 -> get >>= \a -> return (DNMReqRessourceMsg a)
        _ -> error "DNodeRequestMessage: wrong encoding"


data DNodeResponseMessage
  = DNMRspOk
  | DNMRspPing [(DRessourceId,Bool)]
  | DNMRspError String
  deriving (Show)
  
instance Binary DNodeResponseMessage where
  put(DNMRspOk)      = putWord8 1
  put(DNMRspPing rs) = putWord8 2 >> put rs
  put(DNMRspError e) = putWord8 3 >> put e
  get
    = do
      t <- getWord8
      case t of
        1 -> return (DNMRspOk)
        2 -> get >>= \rs -> return (DNMRspPing rs)
        3 -> get >>= \e -> return (DNMRspError e)
        _ -> error "DNodeResponseMessage: wrong encoding"

-- ----------------------------------------------------------------------------

    
-- | Delegates new incomming messages on a unix-socket to their streams.
dispatcher :: Handle -> IO ()
dispatcher hdl
  = do
    debugM localLogger "dispatcher: reading message from connection"
    raw <- getByteStringMessage hdl
    debugM localLogger "dispatcher: message received starting dispatching"
    let msg      = (decode raw)::DNodeRequestMessage
    let sender   = dnm_Req_Sender msg
    let receiver = dnm_Req_Receiver msg
    -- check id and add new addresses
    isValid <- checkDNodeRequest sender receiver
    if isValid
      then do
        debugM localLogger $ "dispatcher: Message: " ++ show msg
        case (dnm_Req_Message msg) of
          -- (DNMReqRegister dna) -> handleRegister dna hdl
          -- (DNMReqUnregister i) -> handleUnregister i hdl
          (DNMReqPing i rs)      -> handlePing i rs hdl
          (DNMReqRessourceMsg a) -> handleRessourceMessage (dna_Id sender) a hdl
      else do
        warningM localLogger $ "message for other node received... dropping request: " ++ show msg
        putByteStringMessage (encode $ DNMRspError "unknown receiver") hdl

{-
requestRegister :: Handle -> IO ()
requestRegister hdl
  = do
    dnd <- readMVar myDNode
    putMessage (encode $ DNMReqRegister $ dnd_Address dnd) hdl
    -- get the response
    raw <- getMessage hdl
    let rsp = (decode raw)::DNodeResponseMessage
    case rsp of
      (DNMRspOk)      -> return ()
      (DNMRspError e) -> error e
      _               -> error "false response"


handleRegister :: DNodeAddress -> Handle -> IO ()
handleRegister dan hdl
  = do
    addNode dan
    -- put the response
    putMessage (encode $ DNMRspOk) hdl
    

requestUnregister :: Handle -> IO ()
requestUnregister hdl
  = do
    dnd <- readMVar myDNode
    let i = dna_Id $ dnd_Address dnd
    putMessage (encode $ DNMReqUnregister i) hdl
    -- get the response
    raw <- getMessage hdl
    let rsp = (decode raw)::DNodeResponseMessage
    case rsp of
      (DNMRspOk)      -> return ()
      (DNMRspError e) -> error e
      _               -> error "false response"
 

handleUnregister :: DNodeId -> Handle -> IO ()
handleUnregister i hdl
  = do
    deleteNode i
    -- put the response
    putMessage (encode $ DNMRspOk) hdl
-}

requestPing :: DNodeAddress -> DNodeAddress -> DNodeId -> [DRessourceId] -> Handle -> IO (Maybe [(DRessourceId, Bool)])
requestPing s r i rs hdl
  = do
    let request = DNodeRequestMessage s r (DNMReqPing i rs)
    putByteStringMessage (encode $ request) hdl
    raw <- getByteStringMessage hdl
    let rsp = (decode raw)::DNodeResponseMessage
    case rsp of
      (DNMRspOk)      -> return (Just [])
      (DNMRspPing ls) -> return (Just ls)
      (DNMRspError e) -> do
        errorM localLogger e
        return Nothing


handlePing :: DNodeId -> [DRessourceId] -> Handle -> IO ()
handlePing otherDni rs hdl
  = do
    dnd <- readMVar myDNode
    let myDni = dna_Id $ dnd_Address dnd
    if (myDni == otherDni)
      then do 
        let orm = dnd_OwnRessourceMap dnd
            ls  = map (\i -> (i, isJust $ Map.lookup i orm)) rs
        putByteStringMessage (encode $ DNMRspPing ls) hdl
      else do
        putByteStringMessage (encode $ DNMRspError "false ping - ids do not match") hdl

      
requestRessourceMessage :: DNodeAddress -> DNodeAddress -> DRessourceAddress -> (Handle -> IO a) -> Handle -> IO a
requestRessourceMessage s r dra requester hdl
  = do
    let request = DNodeRequestMessage s r (DNMReqRessourceMsg dra)
    -- ask node for ressource
    debugM localLogger "requestRessourceMessage: asking node for ressource"
    putByteStringMessage (encode $ request) hdl
    -- get the response
    debugM localLogger "requestRessourceMessage: getting the response"
    raw <- getByteStringMessage hdl
    debugM localLogger "requestRessourceMessage: parsing the response"
    let rsp = (decode raw)::DNodeResponseMessage
    case rsp of
      (DNMRspOk)      -> do requester hdl
      (DNMRspError e) -> error e
      _               -> error "false response"
-- TODO throw real exception here... or something else
      
handleRessourceMessage :: DNodeId -> DRessourceAddress -> Handle -> IO ()
handleRessourceMessage sender dra hdl
  = do
    let i = dra_Id dra
    mbDrd <- lookupRessource i
    case mbDrd of
      (Just drd) ->
        do
        putByteStringMessage (encode $ DNMRspOk) hdl
        let handler = dre_Dispatcher drd
        handler sender hdl
      (Nothing) -> 
        putByteStringMessage (encode $ DNMRspError "ressource not found") hdl
      
-- ----------------------------------------------------------------------------
-- always returns...
addForeignDNode :: DNodeAddress -> IO ()
addForeignDNode dna
  = do
    -- let hn = dna_HostName dna
    --     po = dna_PortNumber dna
    -- performSafeSendRequest requestRegister () hn po
    addNode dna 


-- always returns...
delForeignDNode :: DNodeId -> IO ()
delForeignDNode i
  = do
    mDne <- lookupNode i
    case mDne of
      (Just _) ->
        do
        -- let dna = dne_Address dne
        --     hn  = dna_HostName dna
        --     po  = dna_PortNumber dna
        -- performSafeSendRequest requestUnregister () hn po
        deleteNode i
      (Nothing) -> return ()


-- always returns... if other dnode is not reachable, the function return false
checkForeignDNode :: DNodeId -> IO (Bool)
checkForeignDNode dni
  = do
    (myDna, mbOtherDna) <- lookupDNodeReqestInfo dni    
    case mbOtherDna of
      (Just otherDna) ->
        do
        let hn  = dna_HostName otherDna
            po  = dna_ServiceName otherDna
        res <- performSafeSendRequest (requestPing myDna otherDna dni []) Nothing hn po
        return $ isJust res
      (Nothing) -> return False


addForeignDNodeHandler :: Bool -> DNodeId -> DHandlerFunction -> IO (Maybe DHandlerId)
addForeignDNodeHandler positive dni f
  = modifyMVar myDNode $ \dnd ->
      do
      case (lookupNodeP dni dnd) of
        (Just dne) ->
          do
          let oldTid = dne_PingThreadId dne
          -- do we have an existing ping thread?
          tid <- if (isNothing oldTid)
            then do 
              t <- startPingThread dni
              return $ Just t
            else return oldTid
          uid <- newUnique
          let dhi  = DHandlerId uid dni Nothing
              dne' = dne { dne_HandlerFunctions = Map.insert dhi f (dne_HandlerFunctions dne)
                         , dne_PositiveNodeHandlers = if positive 
                                                        then Set.insert dhi (dne_PositiveNodeHandlers dne)
                                                        else (dne_PositiveNodeHandlers dne)
                         , dne_NegativeNodeHandlers = if positive
                                                        then (dne_NegativeNodeHandlers dne)
                                                        else Set.insert dhi (dne_NegativeNodeHandlers dne)
                         , dne_PingThreadId = tid }
          return ((addNodeP dni dne' dnd), Just dhi)
        (Nothing) -> return (dnd, Nothing)


addForeignDRessourceHandler :: Bool -> DRessourceAddress -> DHandlerFunction -> IO (Maybe DHandlerId)
addForeignDRessourceHandler positive dra f
  = modifyMVar myDNode $ \dnd ->
      do
      let dni   = dra_NodeId dra
          dri   = dra_Id dra
      case (lookupNodeP dni dnd) of
        (Just dne) ->
          do
          let oldTid = dne_PingThreadId dne
          -- do we have an existing ping thread?
          tid <- if (isNothing $ oldTid)
            then do 
              t <- startPingThread dni
              return $ Just t
            else return oldTid
          uid <- newUnique
          let dhi  = DHandlerId uid dni (Just dri)
              dne' = dne { dne_HandlerFunctions = Map.insert dhi f (dne_HandlerFunctions dne)
                         , dne_PositiveResourceHandlers = if positive 
                                                            then Set.insert dhi (dne_PositiveResourceHandlers dne)
                                                            else (dne_PositiveResourceHandlers dne)
                         , dne_NegativeResourceHandlers = if positive
                                                            then (dne_NegativeResourceHandlers dne)
                                                            else Set.insert dhi (dne_NegativeResourceHandlers dne)
                         , dne_PingThreadId = tid }
          return ((addNodeP dni dne' dnd), Just dhi)
        (Nothing) -> return (dnd, Nothing)


delForeignHandler :: DHandlerId -> IO ()
delForeignHandler dhi
  = do
    modifyMVar_ myDNode $ \dnd ->
      do
      let dni   = dhi_DNodeId dhi
      case (lookupNodeP dni dnd) of
        (Just dne) ->
          do
          let dne' = dne { dne_HandlerFunctions = Map.delete dhi (dne_HandlerFunctions dne)
                     , dne_TriggeredHandlers = Set.delete dhi (dne_TriggeredHandlers dne)
                     , dne_PositiveResourceHandlers = Set.delete dhi (dne_PositiveResourceHandlers dne)
                     , dne_NegativeResourceHandlers = Set.delete dhi (dne_NegativeResourceHandlers dne)
                     , dne_PositiveNodeHandlers = Set.delete dhi (dne_PositiveNodeHandlers dne)
                     , dne_NegativeNodeHandlers = Set.delete dhi (dne_NegativeNodeHandlers dne)
                     }
          return $ addNodeP dni dne' dnd
        (Nothing) -> return dnd


genLocalRessourceAddress :: DRessourceType -> String -> IO DRessourceAddress
genLocalRessourceAddress t s
  = do
    i <- genDRessourceId t s
    dnd <- readMVar myDNode
    return $ DRessourceAddress i (dna_Id $ dnd_Address dnd)
    

addLocalRessource :: DRessourceAddress -> DRessourceEntry -> IO ()
addLocalRessource a d
  = do
    let i = dra_Id a
    addRessource i d


delLocalRessource :: DRessourceAddress -> IO ()
delLocalRessource a
  = do
    let i = dra_Id a
    deleteRessource i


delForeignRessource :: DRessourceAddress -> IO ()
delForeignRessource dra
  = modifyMVar_ myDNode $ \dnd ->
      do
      let dni = dra_NodeId dra
          dri = dra_Id dra
      case (lookupNodeP dni dnd) of
        (Just dne) ->
          do
          let handlerIds = Set.fromList $ filter (\dhi -> (dhi_DRessourceId dhi) == (Just dri)) $ Map.keys (dne_HandlerFunctions dne)
              dne'   = dne { dne_HandlerFunctions = Map.filterWithKey (\k _ -> Set.notMember k handlerIds) (dne_HandlerFunctions dne)
                           , dne_TriggeredHandlers = Set.difference (dne_TriggeredHandlers dne) handlerIds
                           , dne_PositiveResourceHandlers = Set.difference (dne_PositiveResourceHandlers dne) handlerIds
                           , dne_NegativeResourceHandlers = Set.difference (dne_NegativeResourceHandlers dne) handlerIds
                           }
          return $ addNodeP dni dne' dnd
        (Nothing) -> return dnd


safeAccessForeignRessource :: DRessourceAddress -> (Handle -> IO a) -> IO a
safeAccessForeignRessource dra requester
  = do
    debugM localLogger $ "accessForeignRessource: " ++ show dra 
    let dni = dra_NodeId dra
    (myDna, mbOtherDna) <- lookupDNodeReqestInfo dni    
    case mbOtherDna of
      (Just otherDna) ->
        do
        debugM localLogger $ "accessForeignRessource: node in list found"
        let hn  = dna_HostName otherDna
            po  = dna_ServiceName otherDna
        mbres <- performMaybeSendRequest (requestRessourceMessage myDna otherDna dra requester) hn po
        case mbres of
          (Just res) -> return res
          (Nothing)  -> retryAccess
      (Nothing) -> retryAccess
    where
      retryAccess
        = do
          dnd <- readMVar myDNode
          threadDelay $ dnd_AccessDelay dnd
          yield
          safeAccessForeignRessource dra requester


unsafeAccessForeignRessource :: DRessourceAddress -> (Handle -> IO a) -> IO a
unsafeAccessForeignRessource dra requester
  = do
    debugM localLogger $ "accessForeignRessource: " ++ show dra 
    let dni = dra_NodeId dra
    (myDna, mbOtherDna) <- lookupDNodeReqestInfo dni    
    case mbOtherDna of
      (Just otherDna) ->
        do
        debugM localLogger $ "accessForeignRessource: node in list found"
        let hn  = dna_HostName otherDna
            po  = dna_ServiceName otherDna
        catch (performUnsafeSendRequest (requestRessourceMessage myDna otherDna dra requester) hn po)
          (\(e ::IOException) -> 
            do
            debugM localLogger $  show e
            throwIO $ DistributedException (show e) "unsafeAccessForeignRessource" "DNode")
      (Nothing) -> 
        throwIO $ DistributedException "node not registered" "unsafeAccessForeignRessource" "DNode"
    

startPingThread :: DNodeId -> IO ThreadId
startPingThread otherDNodeId
  = do
    forkIO $ pingLoop
    where
    -- the function for the main ping loop
    pingLoop = do
      myDnd <- readMVar myDNode
      let myDna = dnd_Address myDnd
      -- get NodeData from local DNode
      mbDne <- lookupNode otherDNodeId
      case mbDne of
        (Just dne) -> do
          myTid <- myThreadId
          if ((Just myTid) == (dne_PingThreadId dne))
            then do
              -- do the pinging
              pingRes <- doPinging myDna dne
              -- evaluate the results and collect the handlers to execute
              (hdls,pingDelay) <- evaluatePingResult pingRes
              -- execute handlers
              sequence_ $ map (\(dhi,f) -> forkIO $ f dhi) hdls
              -- wait and redo the pinging
              threadDelay pingDelay
              pingLoop
            else do
              debugM localLogger $ "the thread ids don't match - leaving thread" ++ show otherDNodeId
              return ()
        (Nothing) -> do
          debugM localLogger $ "ressource not found - leaving thread: " ++ show otherDNodeId
          return ()
    -- the function which does the pinging of the external node
    doPinging myDna dne = do
      let resources = mapMaybe (dhi_DRessourceId) $ Map.keys $ dne_HandlerFunctions dne
          otherDna  = dne_Address dne
          hn        = dna_HostName otherDna
          po        = dna_ServiceName otherDna
      performSafeSendRequest (requestPing myDna otherDna otherDNodeId resources) Nothing hn po
    -- the function which evaluates the result of the ping (changes state of local DNode)
    evaluatePingResult res =
      modifyMVar myDNode $ \dnd -> do
        let pingDelay = dnd_PingDelay dnd
        (dnd', hdls) <- case (Map.lookup otherDNodeId (dnd_NodeMap dnd)) of
          -- the node entry still exists
          (Just dne) -> do    
            let tid            = if (hasAnyHandlers dne)then (dne_PingThreadId dne) else Nothing
                allPosNodeHdls = dne_PositiveNodeHandlers dne
                allPosResHdls  = dne_PositiveResourceHandlers dne
                allNegNodeHdls = dne_NegativeNodeHandlers dne
                allNegResHdls  = dne_NegativeResourceHandlers dne
                trigHdls       = dne_TriggeredHandlers dne
                hdlFuncs       = dne_HandlerFunctions dne
            (allTrigHdls, hdls) <- case (res) of
              -- we've got a response with the list of ressources
              (Just ls) -> do
                let untrigPosNodeHdls = Set.difference allPosNodeHdls trigHdls
                    existingRes = Set.fromList $ map fst $ filter (snd) ls
                    missingRes = Set.fromList $ map fst $ filter (not . snd) ls
                    existingPosResHdls = Set.filter (isMatchingResHdl existingRes) allPosResHdls
                    missingPosResHdls = Set.filter (isMatchingResHdl missingRes) allPosResHdls
                    existingNegResHdls = Set.filter (isMatchingResHdl existingRes) allNegResHdls
                    missingNegResHdls = Set.filter (isMatchingResHdl missingRes) allNegResHdls
                    untrigPosResHdls = Set.difference existingPosResHdls trigHdls
                    untrigNegResHdls = Set.difference missingNegResHdls trigHdls
                    untrigHdls = Set.unions [untrigPosNodeHdls, untrigPosResHdls, untrigNegResHdls]
                    reUntrigHdls = Set.unions [allNegNodeHdls, missingPosResHdls, existingNegResHdls]
                    -- new triggered handler set
                    allTrigHdls = Set.union untrigHdls $ Set.difference trigHdls reUntrigHdls
                    -- list of Handlers to execute
                    hdls = filter (\(dhi,_) -> Set.member dhi untrigHdls) $ Map.toList hdlFuncs
                return (allTrigHdls, hdls)
              -- external node was not reachable
              (Nothing) -> do
                let untrigNegNodeHdls = Set.difference allNegNodeHdls trigHdls
                    untrigNegResHdls = Set.difference allNegResHdls trigHdls
                    untrigHdls = Set.union untrigNegNodeHdls untrigNegResHdls
                    -- new triggered handler set
                    allTrigHdls = Set.union allNegNodeHdls allNegResHdls
                    -- list of Handlers to execute
                    hdls = filter (\(dhi,_) -> Set.member dhi untrigHdls) $ Map.toList hdlFuncs
                return (allTrigHdls, hdls)
            let dne' = dne { dne_PingThreadId = tid, dne_TriggeredHandlers = allTrigHdls }
                dnd' = addNodeP otherDNodeId dne' dnd
            return (dnd', hdls)
          -- in the meantime, the node entry was deleted
          (Nothing) -> return (dnd, [])
        return (dnd', (hdls, pingDelay))
    isMatchingResHdl :: Set.Set DRessourceId -> DHandlerId -> Bool
    isMatchingResHdl resIds dhi = if (isJust mbResId) then isMember else False
      where
        mbResId = dhi_DRessourceId dhi
        resId = fromJust mbResId
        isMember = Set.member resId resIds
    hasAnyHandlers :: DNodeEntry -> Bool
    hasAnyHandlers dne = not $ Map.null (dne_HandlerFunctions dne)

