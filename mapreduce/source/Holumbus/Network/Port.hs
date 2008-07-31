-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Network.Port
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Stream and Port datatype for internal an external process communication.
  Useful for communikation of distributed systems.
  
-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Holumbus.Network.Port
(
-- * Constants
  time1
, time10
, time30
, time60
, time120

-- * Datatypes
, SocketId(..) -- reexport from core
, MessageType
, Message
, StreamName
, StreamType(..)
, Stream
, Port

-- * Message-Operations
, getMessageType
, getMessageData
, getGenericData

-- * Stream-Operations
, newGlobalStream
, newLocalStream
, newPrivateStream
, newStream
, closeStream

, isEmptyStream
, readStream
, readStreamMsg
, tryReadStream
, tryReadStreamMsg
, tryWaitReadStream
, tryWaitReadStreamMsg
, withStream

-- * Port-Operations
, newPortFromStream
, newPort
, newGlobalPort

, isPortLocal
, send
, sendWithGeneric
, sendWithMaybeGeneric 

, writePortToFile
, readPortFromFile

-- * Debug
, printStreamController
)
where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Binary
import qualified Data.ByteString.Lazy as B
import           Data.Char
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Time
import           Network
import           System.IO
import           System.IO.Unsafe
import           System.Log.Logger
import           System.Timeout

import           Text.XML.HXT.Arrow

import           Holumbus.Network.Site
import           Holumbus.Network.Core
import qualified Holumbus.Data.MultiMap as MMap


localLogger :: String
localLogger = "Holumbus.Network.Port"



-- -----------------------------------------------------------------------------
-- Constants
-- -----------------------------------------------------------------------------


-- | one second
time1 :: Int
time1 = 1000000

-- | 10 seconds
time10 :: Int
time10 = 10000000

-- | 30 seconds
time30 :: Int
time30 = 30000000


-- | 60 seconds
time60 :: Int
time60 = 60000000


-- | 120 seconds
time120 :: Int
time120 = 120000000



defaultPort :: PortNumber
defaultPort = 9000


maxPort :: PortNumber
maxPort = 40000




-- ----------------------------------------------------------------------------
-- Message-Datatype
-- ----------------------------------------------------------------------------

-- | Message Type
--   Is it an internal Message or does it come from an external Node?
data MessageType = MTInternal | MTExternal 
  deriving (Show)

instance Binary MessageType where
  put MTInternal = putWord8 1
  put MTExternal = putWord8 2
  get
    = do
      t <- getWord8
      case t of
        1 -> return MTInternal
        _ -> return MTExternal      


-- | Message Datatype.
--   We are sending additional information, to do debugging
data (Show a, Binary a) => Message a = Message {
    msg_Type           :: ! MessageType          -- ^ the message-type
  , msg_Receiver       :: ! StreamName
  , msg_Data           :: ! a                    -- ^ the data  
  , msg_Generic        :: ! (Maybe B.ByteString) -- ^ some generic data -- could be another port
  , msg_ReceiverSocket :: ! (Maybe SocketId)     -- ^ socket to which the message is send (DEBUG)
  , msg_SenderSocket   :: ! (Maybe SocketId)     -- ^ socket from which the message was send (DEBUG)
  , msg_Send_time      :: ! UTCTime              -- ^ timestamp from the sender (DEBUG)
  , msg_Receive_time   :: ! UTCTime              -- ^ timestamp from the receiver (DEBUG)
  } deriving (Show)

instance (Show a, Binary a) => Binary (Message a) where
  put (Message t r d g rs ss t1 t2)
    = do
      put t
      put r
      put d
      put g
      put rs
      put ss
      put $ show t1
      put $ show t2
  get
    = do
      t     <- get
      r     <- get
      d     <- get
      g     <- get
      rs    <- get
      ss    <- get
      t1Str <- get
      t2Str <- get
      return $ (Message t r d g rs ss (read t1Str) (read t2Str))




-- ----------------------------------------------------------------------------
-- Port-Datatype
-- ----------------------------------------------------------------------------

data Port a = Port {
    p_StreamName :: StreamName
  , p_SocketId   :: Maybe SocketId
  } deriving (Show)

instance (Show a, Binary a) => Binary (Port a) where
  put (Port sn soid) = put sn >> put soid
  get
    = do
      sn   <- get
      soid <- get
      return (Port sn soid)

instance (Show a, Binary a) => XmlPickler (Port a) where
  xpickle = xpPort
  
xpPort :: PU (Port a)
xpPort = 
    xpElem "port" $
    xpWrap(\(sn, soid) -> Port sn soid, \(Port sn soid) -> (sn, soid)) $
    xpPair (xpAttr "name" xpText) (xpOption $ xpickle)




-- ----------------------------------------------------------------------------
-- Stream-Datatype
-- ----------------------------------------------------------------------------

type StreamName = String

data StreamType = STGlobal | STLocal | STPrivate
  deriving (Show, Eq, Ord)

data StreamState = SSOpen |SSClosed
  deriving (Show, Eq, Ord)

-- | The stream datatype
data Stream a
   = Stream {
      s_StreamName :: StreamName
    , s_SocketId   :: SocketId
    , s_State      :: MVar StreamState
    , s_Type       :: StreamType
    , s_Channel    :: BinaryChannel
    }

instance (Show a, Binary a) => Show (Stream a) where
  show (Stream sn soid _ st _)
    = "(Stream" ++
      " - Name: " ++ show sn ++
      " - SocketId: " ++ show soid ++
      " - Type: " ++ show st ++
      " )"


  
-- ----------------------------------------------------------------------------
-- StreamController-Datatype
-- ----------------------------------------------------------------------------

type BinaryChannel = (Chan (Message B.ByteString))

data StreamControllerData = StreamControllerData {
    scd_DefaultSocket :: Maybe SocketId
  , scd_StreamId      :: Int
  , scd_StreamMap     :: Map.Map StreamName (MVar StreamState, BinaryChannel, PortNumber, Bool)
  , scd_ServerMap     :: Map.Map PortNumber (ThreadId, HostName)
  , scd_PortMap       :: MMap.MultiMap PortNumber StreamName 
  }

instance Show StreamControllerData where
  show (StreamControllerData ds si stm sem pom)
    =  "StreamControllerData:\n"
    ++ "  DefaultSocket:\t" ++ show ds
    ++ "  lastStreamId:\t" ++ show si
    ++ "  Streams:\t" ++ show (Map.keys stm)
    ++ "  ServerMap:\t" ++ show sem
    ++ "  PortMap:\t" ++ show pom

type StreamController = MVar StreamControllerData




-- ----------------------------------------------------------------------------
-- StreamController-Operations
-- ----------------------------------------------------------------------------

{-# NOINLINE streamController #-}
streamController :: StreamController
streamController
  = do
    unsafePerformIO $ newMVar emptyStreamControllerData
    where
      emptyStreamControllerData
        = StreamControllerData
            Nothing
            0
            Map.empty
            Map.empty 
            MMap.empty 

-- TODO doppelte Stream-Namen abfangen
getNextStreamName :: IO (StreamName)
getNextStreamName 
  = modifyMVar streamController $
      \scd ->
      do
      let i    = (scd_StreamId scd) + 1
      let n    = "$" ++ show i
      let scd' = (scd {scd_StreamId = i})
      return (scd',n)


isValidStreamName :: StreamName -> Bool
isValidStreamName [] = False
isValidStreamName sn = null $ filter isForbiddenChar sn 
  where
  isForbiddenChar c = not $ isAlphaNum c


validateStreamName :: Maybe StreamName -> IO (StreamName)
validateStreamName (Nothing) = getNextStreamName
validateStreamName (Just sn)
  | isValidStreamName sn 
      = do
        taken <- withMVar streamController $
          \scd -> return $ Map.member sn (scd_StreamMap scd) 
        if (taken) 
          then do error "stream name already exists"
          else do return sn 
  | otherwise 
      = error "invalid stream name"


openSocket :: Maybe PortNumber -> IO (SocketId)
-- get/start the default socket
openSocket (Nothing)
  = modifyMVar streamController $
      \scd ->
      do
      case (scd_DefaultSocket scd) of
        (Nothing) ->
          do
          res <- startStreamServer defaultPort maxPort
          case res of
            (Nothing) -> 
              error "Port: getDefaultPort: unable to open defaultsocket"
            (Just (soid@(SocketId hn pn), tid)) ->
              do
              let serverMap = Map.insert pn (tid,hn) (scd_ServerMap scd)
              let scd' = scd {scd_DefaultSocket = (Just soid), scd_ServerMap = serverMap}
              return (scd',soid)
        (Just soid) -> 
          return (scd,soid)
-- get/start the new socket
openSocket (Just pn)
  = modifyMVar streamController $
      \scd ->
      do
      let mp = Map.lookup pn (scd_ServerMap scd)
      case mp of
        (Nothing) ->
          do
          res <- startStreamServer pn pn
          case res of
            (Nothing) -> 
              error "Port: getDefaultPort: unable to open socket"
            (Just (soid@(SocketId hn _), tid)) ->
              do
              let serverMap = Map.insert pn (tid,hn) (scd_ServerMap scd)
              let scd' = scd {scd_ServerMap = serverMap}
              return (scd',soid)
        (Just (_,hn)) ->
          return (scd,SocketId hn pn)
         

closeSocket :: SocketId -> IO ()
closeSocket soid@(SocketId _ pn)
  = modifyMVar streamController $
      \scd ->
      do
      let ds =  (scd_DefaultSocket scd)
      if (isNothing ds || soid == fromJust ds)
        -- don't delete the defaultSocket
        then do
          return (scd,())
        else do
          if (MMap.member pn (scd_PortMap scd))
            -- if the port is still used by other streams, keep it
            then do
              return (scd,())
            -- close the socket and delete it from the controller
            else do
              let (tid, _) = fromJust $ Map.lookup pn (scd_ServerMap scd)
              stopStreamServer tid
              let serverMap = Map.delete pn (scd_ServerMap scd)
              return (scd {scd_ServerMap = serverMap},())
      

registerStream :: Stream a ->  IO ()
registerStream s
  = modifyMVar streamController $
      \scd ->
      do
      let priv      = STPrivate == (s_Type s)
      let streamMap = Map.insert sn (ss,ch,pn,priv) (scd_StreamMap scd)
      let portMap   = MMap.insert pn sn (scd_PortMap scd) 
      return (scd { scd_StreamMap = streamMap, scd_PortMap = portMap }, ())
    where      
    (SocketId _ pn) = s_SocketId s
    ch = s_Channel s
    sn = s_StreamName s
    ss = s_State s


unregisterStream :: Stream a -> IO ()
unregisterStream s
  = modifyMVar streamController $
      \scd ->
      do
      let streamMap = Map.delete sn (scd_StreamMap scd)
      let portMap   = MMap.deleteElem pn sn (scd_PortMap scd) 
      return (scd { scd_StreamMap = streamMap, scd_PortMap = portMap }, ())
    where      
    (SocketId _ pn) = s_SocketId s
    sn = s_StreamName s


getStreamNamesForPort :: PortNumber -> IO (Set.Set StreamName)
getStreamNamesForPort pn
  = withMVar streamController $
      \scd -> return $ MMap.lookup pn (scd_PortMap scd)


getStreamData :: StreamName -> IO (Maybe (MVar StreamState, BinaryChannel, PortNumber, Bool))
getStreamData sn
  = withMVar streamController $
      \scd -> return $ Map.lookup sn (scd_StreamMap scd)

{-
getStateForStream :: StreamName -> IO (Maybe StreamState)
getStateForStream sn 
  = withMVar streamController $
      \scd -> 
      do 
      let sd = Map.lookup sn (scd_StreamMap scd)
      case sd of
        (Nothing) -> return Nothing
        (Just (mVarSS,_)) -> withMVar mVarSS $ \ss -> return $ Just ss
-}



-- -----------------------------------------------------------------------------
-- Message-Operations
-- -----------------------------------------------------------------------------

encodeMessage :: (Show a, Binary a) => Message a -> Message B.ByteString
encodeMessage (Message t n d g r s t1 t2) = (Message t n (encode d) g r s t1 t2)


decodeMessage :: (Show a, Binary a) => Message B.ByteString -> Message a
decodeMessage (Message t n d g r s t1 t2) = (Message t n (decode d) g r s t1 t2)

        
newMessage :: (Show a, Binary a) => MessageType -> StreamName -> a -> Maybe B.ByteString -> IO (Message a)
newMessage t n d g
  = do
    time <- getCurrentTime
    return (Message t n d g Nothing Nothing time time)


getMessageType :: (Show a, Binary a) => Message a -> MessageType
getMessageType = msg_Type


getMessageData :: (Show a, Binary a) => Message a -> a
getMessageData = msg_Data


getGenericData :: (Show a, Binary a) => Message a -> (Maybe B.ByteString)
getGenericData = msg_Generic


getMessageReceiver :: (Show a, Binary a) => Message a -> StreamName
getMessageReceiver = msg_Receiver


updateReceiveTime :: (Show a, Binary a) => Message a -> IO (Message a)
updateReceiveTime msg 
  = do
    time <- getCurrentTime
    return (msg {msg_Receive_time = time})

    
updateReceiverSocket :: (Show a, Binary a) => Message a -> SocketId -> Message a
updateReceiverSocket msg soId = msg { msg_ReceiverSocket = Just soId }


updateSenderSocket :: (Show a, Binary a) => Message a -> SocketId -> Message a
updateSenderSocket msg soId = msg { msg_SenderSocket = Just soId } 




-- ----------------------------------------------------------------------------
-- Stream-Operations
-- ----------------------------------------------------------------------------


newGlobalStream :: (Show a, Binary a) => StreamName -> IO (Stream a)
newGlobalStream n = newStream STGlobal (Just n) Nothing


newLocalStream :: (Show a, Binary a) => Maybe StreamName -> IO (Stream a)
newLocalStream sn = newStream STLocal sn Nothing


newPrivateStream :: (Show a, Binary a) => Maybe StreamName -> IO (Stream a)
newPrivateStream sn = newStream STPrivate sn Nothing


newStream 
  :: (Show a, Binary a) 
  => StreamType -> Maybe StreamName -> Maybe PortNumber 
  -> IO (Stream a)
newStream STGlobal Nothing _ = error "newStream: global ports always need a name"
newStream st n pn
  = do
    ch <- newChan
    ss <- newMVar SSOpen
    sn <- validateStreamName n
    soid <- openSocket pn
    let s = Stream sn soid ss st ch
    registerStream s
    return s


closeStream :: (Show a, Binary a) => Stream a -> IO ()
closeStream s
  = do
    -- set the stream closed
    modifyMVar (s_State s) $ \_ -> return (SSClosed,())
    unregisterStream s
    closeSocket (s_SocketId s)


-- | Writes a message to the channel of the stream.
--   This function is not public, so you have to write to a stream throug its
--   port
writeChannel :: Chan (Message B.ByteString) -> Message B.ByteString -> IO ()
writeChannel ch msg
  = do
    newMsg <- updateReceiveTime msg
    writeChan ch newMsg


isEmptyStream :: Stream a -> IO Bool
isEmptyStream s
  = do
    isEmptyChan (s_Channel s)


readStream :: (Show a, Binary a) => Stream a -> IO a
readStream s
  = do
    msg <- readStreamMsg s
    return (msg_Data msg)


readStreamMsg :: (Show a, Binary a) => Stream a -> IO (Message a)
readStreamMsg s
  = do
    debugM localLogger "PORT: readStreamMsg 1"
    res <- readChan (s_Channel s)
    debugM localLogger "PORT: readStreamMsg 2"
    return $ decodeMessage res


--TODO is there a better way for non-blocking?
tryStreamAction :: Stream a -> (Stream a -> IO (b)) -> IO (Maybe b)
tryStreamAction s f
  = do
    empty <- isEmptyStream s
    if (not empty) 
      then do timeout 1000 (f s)
      else do return Nothing


tryWaitStreamAction :: Stream a -> (Stream a -> IO (b)) -> Int -> IO (Maybe b)
tryWaitStreamAction s f t
  = do
    debugM localLogger "tryWaitStreamAction: waiting..."
    r <- timeout t (f s)
    debugM localLogger "tryWaitStreamAction: ...finished"
    let debugResult = maybe "nothing received" (\_ -> "value found") r
    debugM localLogger $ "tryWaitStreamAction: " ++ debugResult
    return r
    
    
tryReadStream :: (Show a, Binary a) => Stream a -> IO (Maybe a)
tryReadStream s
  = do
    tryStreamAction s readStream

    
tryReadStreamMsg :: (Show a, Binary a) => Stream a -> IO (Maybe (Message a))
tryReadStreamMsg s
  = do
    tryStreamAction s readStreamMsg


tryWaitReadStream :: (Show a, Binary a) => Stream a -> Int -> IO (Maybe a)
tryWaitReadStream s t 
  = do
    tryWaitStreamAction s readStream t

    
tryWaitReadStreamMsg :: (Show a, Binary a) => Stream a -> Int -> IO (Maybe (Message a))
tryWaitReadStreamMsg s t
  = do
    tryWaitStreamAction s readStreamMsg t


-- | Encapsulates a stream.
--   A new stream is created, then some user-action is done an after that the
--   stream is closed. 
withStream :: (Show a, Binary a) => (Stream a -> IO b) -> IO b
withStream f
  = do
    s <- newStream STLocal Nothing Nothing
    res <- f s
    closeStream s
    return res




-- ----------------------------------------------------------------------------
-- StreamServer-Operations
-- ----------------------------------------------------------------------------

startStreamServer :: PortNumber -> PortNumber -> IO (Maybe (SocketId, ThreadId))
startStreamServer actPo maxPo
  = do
    res <- startSocket (streamDispatcher) actPo maxPo
    case res of
      Nothing ->
        return Nothing
      (Just (tid, hn, po)) ->
        return (Just (SocketId hn po, tid))
    

stopStreamServer :: ThreadId -> IO ()
stopStreamServer sId 
  = do
    me <- myThreadId
    debugM localLogger $ "stopping server... with threadId: " ++ show sId ++ " - form threadId: " ++ show me
    throwDynTo sId me
    yield


streamDispatcher :: SocketId -> Handle -> HostName -> PortNumber -> IO ()
streamDispatcher (SocketId _ ownPo) hdl hn po
  = do
    debugM localLogger "streamDispatcher: getting message from handle"
    raw <- getMessage hdl
    let msg = (decode raw)::Message B.ByteString
    debugM localLogger $ "streamDispatcher: Message: " ++ show msg
    let sn = getMessageReceiver msg
    sns <- getStreamNamesForPort ownPo
    sd <- getStreamData sn
    if (Set.member sn sns)
      -- if the socket knows the stream 
      then do
        case sd of
          (Just (_,_,_,True)) ->
            warningM localLogger $ "streamDispatcher: received msg for private stream " ++ sn
          (Just (_,ch,_,_)) ->
            do
            debugM localLogger "streamDispatcher: writing message to channel"
            writeChannel ch $ updateSenderSocket msg (SocketId hn po)
            debugM localLogger "streamDispatcher: message written to channel"
          (Nothing) ->
            do
            errorM localLogger $ "streamDispatcher: no channel for stream " ++ sn
      -- if the stream is unknown, log this as error
      else do
        warningM localLogger $ "streamDispatcher: received msg for unknown stream " ++ sn
    
{-
putMessage :: Message B.ByteString -> Handle -> IO ()
putMessage msg hdl
  = do
    handle (\e -> errorM localLogger $ "putMessage: " ++ show e) $ do
      enc <- return (encode msg)
      hPutStrLn hdl ((show $ B.length enc) ++ " ")
      B.hPut hdl enc


getMessage :: Handle -> IO (Message B.ByteString)
getMessage hdl
  = do
    line <- hGetLine hdl
    let pkg = words line
    raw <- B.hGet hdl (read $ head pkg)
    return (decode raw)
-}




-- -----------------------------------------------------------------------------
-- Port Operations  
-- -----------------------------------------------------------------------------


-- | Creates a new Port, which is bound to a stream.
--   This port is internal, you automatically get an external port, if you
--   seriablize the port via the Binary-Class
newPortFromStream :: Stream a -> IO (Port a)
newPortFromStream s
  = do
    let sn = s_StreamName s
    let soid = s_SocketId s
    return $ Port sn (Just soid)


newPort :: StreamName -> HostName -> PortNumber -> IO (Port a)
newPort sn hn po = return $ Port sn (Just $ SocketId hn po)


newGlobalPort :: StreamName -> IO (Port a)
newGlobalPort sn = return $ Port sn Nothing


isPortLocal :: Port a -> IO Bool
isPortLocal (Port sn mbSoid)
  = do
    sd <- getStreamData sn
    case sd of
      (Just (_,_,po,_)) ->
        do
        case mbSoid of
          (Just s1) ->
             do
             sid <- getSiteId
             let hn = getSiteHost sid
             let s2 = SocketId hn po 
             return (s1 == s2)
          (Nothing) ->
             do
             return False
      (Nothing) ->
        return False
  
    
-- | Send data to the stream of the port.
--   The data is send via network, if the stream is located on an external
--   processor
send :: (Show a, Binary a) => Port a -> a -> IO ()
send p d = sendWithMaybeGeneric p d Nothing


-- | Like "send", but here we can give some generic data (e.g. a port for reply 
--   messages).
sendWithGeneric :: (Show a, Binary a) => Port a -> a -> B.ByteString -> IO ()
sendWithGeneric p d rp = sendWithMaybeGeneric p d (Just rp) 


-- | Like "sendWithGeneric", but the generic data is optional
sendWithMaybeGeneric :: (Show a, Binary a) => Port a -> a -> Maybe B.ByteString -> IO ()
sendWithMaybeGeneric p@(Port sn mbsoid) d rp
  = do
    local <- isPortLocal p
    if local
      then do
        msg <- newMessage MTInternal sn d rp
        sd <- getStreamData sn
        case sd of
          (Just (_,ch,_,_)) ->
            do
            writeChannel ch $ encodeMessage msg
          _ ->
            do
            errorM localLogger $ "sendWithMaybeGeneric: no channel found for stream " ++ sn
      else do
        case mbsoid of
          (Just so@(SocketId hn po)) ->
            do
            msg <- newMessage MTExternal sn d rp
            let raw = encode $ encodeMessage $ updateReceiverSocket msg so
            sendRequest (putMessage raw) hn (PortNumber po)
            return ()
          (Nothing) -> undefined


-- | Writes a port-description to a file.
--   Quite useful fpr sharing ports between programs
writePortToFile :: (Show a, Binary a) => Port a -> FilePath -> IO ()
writePortToFile p fp
  = do
    bracket 
      (openFile fp WriteMode)
      (hClose) 
      (\hdl ->
        do 
        enc <- return (encode $ p)
        hPutStrLn hdl ((show $ B.length enc) ++ " ")
        B.hPut hdl enc
      )


-- | Reads a port-description from a file.
--   Quite useful fpr sharing ports between programs
readPortFromFile :: (Show a, Binary a) => FilePath -> IO (Port a)
readPortFromFile fp
  =  do
     bracket
        (openFile fp ReadMode)
        (hClose)
        (\hdl -> 
          do
          pkg <- liftM words $ hGetLine hdl
          raw <- B.hGet hdl (read $ head pkg)
          p <- return (decode raw)
          return p
        )




-- ----------------------------------------------------------------------------
-- Debugging
-- ----------------------------------------------------------------------------


printStreamController :: IO ()
printStreamController
  = withMVar streamController $
      \scd ->
      do
      putStrLn "StreamController:"
      putStrLn $ show scd

