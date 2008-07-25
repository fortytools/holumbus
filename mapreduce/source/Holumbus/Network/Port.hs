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
  
  --TODO remove the "show" instances
  --TODO better exception handling
  --TODO rewrite the broadcast-port with a set, not a list

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
, MessageType
, Message
, Stream
, Port

-- * Message-Operations
, getMessageType
, getMessageData
, getGenericData

-- * initialization and deinitialization
, initializeStreamController
, initializeStreamControllerOnPort
, deinitializeStreamController

-- * Stream-Operations
, newStream
, newNamedStream
, closeStream

, readStream
, readStreamMsg
, tryReadStream
, tryReadStreamMsg
, tryWaitReadStream
, tryWaitReadStreamMsg

, isEmptyStream
, withStream


-- * Port-Operations
, newPort

, send
, sendWithGeneric
, sendWithMaybeGeneric 

, mergePorts

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
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Time
import           Network
import           System.IO
import           System.IO.Unsafe
import           System.Log.Logger
import           System.Timeout

import           Text.XML.HXT.Arrow

import           Holumbus.Network.Site
import qualified Holumbus.Network.Server as Server
import qualified Holumbus.Network.Client as Client


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


-- -----------------------------------------------------------------------------
-- Socket Descriptor
-- -----------------------------------------------------------------------------


-- | All data, that is needed to address a socket.
--   Contains the hostname and the portNumber
data SocketId = SocketId HostName PortNumber 
  deriving (Show, Eq)


instance Binary (SocketId) where
  put (SocketId hn po)
    = put hn >> (put . toInteger) po
  get
    = do
      hn <- get
      poInt <- get
      return (SocketId hn (fromInteger poInt))

      
instance XmlPickler SocketId where
  xpickle = xpSocketId
  
xpSocketId :: PU SocketId
xpSocketId = 
    xpElem "socketId" $
      xpWrap (\(hn, po) -> SocketId hn (fromInteger po), \(SocketId hn po) -> (hn, toInteger po)) xpSockId
      where
      xpSockId =
        xpPair
          (xpElem "hostname" xpText)
          (xpElem "socket" xpickle)

-- -----------------------------------------------------------------------------
-- Message-Datatype
-- -----------------------------------------------------------------------------


-- | Message Type
--   Is it an internal Message or does it come from an external Node?
data MessageType = Internal | External 
  deriving (Show)


-- | Message Datatype.
--   We are sending additional information, to do debugging
data (Show a, Binary a) => Message a = Message {
    msg_Type         :: ! MessageType          -- ^ the message-type
  , msg_ReceiverName :: ! StreamName
  , msg_Data         :: ! a                    -- ^ the data  
  , msg_Generic      :: ! (Maybe B.ByteString) -- ^ some generic data -- could be another port
  , msg_Receiver     :: ! (Maybe SocketId)     -- ^ socket to which the message is send (DEBUG)
  , msg_Sender       :: ! (Maybe SocketId)     -- ^ socket from which the message was send (DEBUG)
  , msg_Send_time    :: ! UTCTime              -- ^ timestamp from the sender (DEBUG)
  , msg_Receive_time :: ! UTCTime              -- ^ timestamp from the receiver (DEBUG)
  } deriving (Show)


instance Binary MessageType where
  put Internal = putWord8 1
  put External = putWord8 2
  get
    = do
      t <- getWord8
      case t of
        1 -> return Internal
        _ -> return External      


instance (Show a, Binary a) => Binary (Message a) where
  put (Message t n d g r s t1 t2)
    = do
      put t
      put n
      put d
      put g
      put r
      put s
      put $ show t1
      put $ show t2
  get
    = do
      typ <- get
      nam <- get
      dat <- get
      gen <- get
      re <- get
      sen <- get
      t1Str <- get
      t2Str <- get
      return $ (Message typ nam dat gen re sen (read t1Str) (read t2Str))
      




-- -----------------------------------------------------------------------------
-- Stream-Datatype
-- -----------------------------------------------------------------------------


-- | The threadId of the server which accepts incomming messages for a stream 
type ServerId = ThreadId

type StreamId = Int

type StreamName = String

type StreamMap = Map.Map StreamName BinaryChannel

type BinaryChannel = (Chan (Message B.ByteString))

data StreamControllerData = StreamControllerData {
    scd_SocketId  :: SocketId
  , scd_SiteId    :: SiteId
  , scd_ServerId  :: ServerId
  , scd_StreamMap :: StreamMap
  , scd_StreamId  :: StreamId
  }

instance Show StreamControllerData where
  show (StreamControllerData so si serv m i)
    =  "StreamControllerData:\n"
    ++ "  SocketId:\t" ++ show so
    ++ "  SiteId:\t" ++ show si
    ++ "  ServerId:\t" ++ show serv
    ++ "  Streams:\t" ++ show (Map.keys m)
    ++ "  lastStreamId:\t" ++ show i

type StreamController = MVar (Maybe StreamControllerData)

-- | The stream datatype
data Stream a
   = Stream {
      s_StreamId :: StreamId
    , s_Name     :: StreamName
    , s_Chan     :: BinaryChannel
    } 


instance Show (Stream a) where
  show (Stream i n _)
    = "(Stream - ID: " ++
      show i ++
      " - StreamName: " ++
      show n ++
      " )"


-- -----------------------------------------------------------------------------
-- Port-Datatype
-- -----------------------------------------------------------------------------      


data Port a
  = SinglePort StreamId StreamName SocketId SiteId    -- ^ single Port
  | BroadcastPort [Port a]                            -- ^ broadcast Port


instance Show (Port a) where
  show (SinglePort i n so si)
    = "(SinglePort - StreamId: " ++ show i ++ " - StreamName: " ++ show n ++
      " - SocketId: " ++ show so ++ " SiteId: " ++ show si ++ " )"
  show (BroadcastPort l)
    = "(BroadcastPort - " ++ (concat . map show) l ++ " )" 


instance Binary (Port a) where
  put (SinglePort i n soId siteId)
    = do
      putWord8 1
      put i
      put n
      put soId
      put siteId
  put (BroadcastPort lst)
    = do
      putWord8 2
      put lst
  get
    = do
      t <- getWord8
      case t of
        1 -> 
          do
          i <- get
          n <- get
          soid <- get
          siteId <- get
          return (SinglePort i n soid siteId)
        _ ->
          do
          lst <- get
          return (BroadcastPort lst)


instance XmlPickler (Port a) where
  xpickle = xpPort
  
xpPort :: PU (Port a)
xpPort = 
    xpElem "port" $
    xpAlt tag ps
      where
      tag (SinglePort _ _ _ _ ) = 0
      tag (BroadcastPort _    ) = 1
      ps = [xpWrap (\(i, n, so, si) -> SinglePort i n so si, \(SinglePort i n so si) -> (i, n, so, si)) xpSinglePort
           ,xpWrap (\ls -> BroadcastPort ls, \(BroadcastPort ls) -> ls ) xpBroadcastPort ]
      xpSinglePort = 
        xp4Tuple
          (xpElem "id" xpickle)
          (xpElem "name" xpText)
          (xpickle)
          (xpickle)
      xpBroadcastPort = 
        xpElem "portList" $ xpList xpPort


-- -----------------------------------------------------------------------------
-- Port-Map
-- -----------------------------------------------------------------------------
     

{-# NOINLINE streamController #-}
streamController :: StreamController
streamController
  = do
    unsafePerformIO $ newMVar Nothing


-- -----------------------------------------------------------------------------
-- Message-Operations
-- -----------------------------------------------------------------------------

        
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


getMessageReceiverName :: (Show a, Binary a) => Message a -> (StreamName)
getMessageReceiverName = msg_ReceiverName


updateReceiveTime :: (Show a, Binary a) => Message a -> IO (Message a)
updateReceiveTime msg 
  = do
    time <- getCurrentTime
    return (msg {msg_Receive_time = time})

    
updateReceiver :: (Show a, Binary a) => Message a -> SocketId -> Message a
updateReceiver msg soId = msg { msg_Receiver = Just soId }


updateSender :: (Show a, Binary a) => Message a -> SocketId -> Message a
updateSender msg soId = msg { msg_Sender = Just soId } 

      
-- -----------------------------------------------------------------------------
-- Initialise and Deinitialise Stream Environment
-- -----------------------------------------------------------------------------

mkStreamControllerData :: PortNumber -> PortNumber -> IO StreamControllerData
mkStreamControllerData dp mp
  = do
    s <- (startStreamServer dp mp)
    let (i, server) = fromJust s
    siteId <- getSiteId
    let scd = StreamControllerData i siteId server Map.empty 0
    return scd


initializeStreamController :: IO ()
initializeStreamController
  = modifyMVar streamController $ 
      \sc -> do
      case sc of 
        (Nothing) -> do
          scd <- mkStreamControllerData defaultPort maxPort
          return (Just scd, ())
        _ -> return (sc, ())
 

initializeStreamControllerOnPort :: PortNumber -> IO ()
initializeStreamControllerOnPort po
  = modifyMVar streamController $ 
      \sc -> do
      case sc of 
        (Nothing) -> do
          scd <- mkStreamControllerData po po
          return (Just scd, ())
        _ -> return (sc, ())


deinitializeStreamController :: IO ()
deinitializeStreamController
 = modifyMVar streamController $ 
      \sc -> do
      case sc of 
        (Just scd) -> do
          stopStreamServer (scd_ServerId scd)
          return (Nothing, ())
        _ -> return (sc, ())   



-- -----------------------------------------------------------------------------
-- Stream Operations  
-- -----------------------------------------------------------------------------


defaultPort :: PortNumber
defaultPort = 9000


maxPort :: PortNumber
maxPort = 40000


newNamedStream :: (Show a, Binary a) => StreamName -> IO (Stream a)
newNamedStream n
  = do
    -- initialize the stream... doesn't matter if it is already done
    initializeStreamController
    createNewStream(Just n)

-- | Create a new stream.
--   The stream is bound to a random port.
newStream :: (Show a, Binary a) => IO (Stream a)
newStream
  = do
    -- initialize the stream... doesn't matter if it is already done
    initializeStreamController
    createNewStream Nothing
  

createNewStream :: (Show a, Binary a) => Maybe StreamName -> IO (Stream a)
createNewStream mbn
  = modifyMVar streamController $
      \mbscd ->
      do
      let scd = fromJust mbscd
      ch <- newChan
      let i    = (scd_StreamId scd) + 1
      let n    = maybe ("$" ++ show i ++ "$") id mbn
      let s    = Stream i n ch 
      let sm   = Map.insert n ch (scd_StreamMap scd)
      let scd' = scd {scd_StreamId = i, scd_StreamMap = sm }
      return (Just scd', s)
    

lookupStreamChannel :: StreamName -> IO (Maybe BinaryChannel)
lookupStreamChannel n 
  = modifyMVar streamController $
      \sc -> do
      case sc of
        (Just scd) -> 
          do
          let ch = Map.lookup n (scd_StreamMap scd)
          return (sc, ch)
        _ -> return (Nothing, Nothing)


-- | Close a stream
--   Only the the socket is closed, if you keep the stream, you can internally
--   send data to it
closeStream :: (Binary a) => Stream a -> IO ()
closeStream s 
  = modifyMVar streamController $
      \sc -> do
      case sc of
        (Just scd) -> deleteStream scd
        _ -> return (Nothing, ())
    where
    deleteStream scd
      = do  
        let sm   = Map.delete (s_Name s) (scd_StreamMap scd)
        let scd' = scd { scd_StreamMap = sm }
        return (Just scd', ())
      
      
encodeMessage :: (Show a, Binary a) => Message a -> Message B.ByteString
encodeMessage (Message t n d g r s t1 t2) = (Message t n (encode d) g r s t1 t2)


decodeMessage :: (Show a, Binary a) => Message B.ByteString -> Message a
decodeMessage (Message t n d g r s t1 t2) = (Message t n (decode d) g r s t1 t2)


-- | Writes a message to the channel of the stream.
--   This function is not public, so you have to write to a stream throug its
--   port
writeChannel :: Chan (Message B.ByteString) -> Message B.ByteString -> IO ()
writeChannel ch msg
  = do
    newMsg <- updateReceiveTime msg
    writeChan ch newMsg


readStream :: (Show a, Binary a) => Stream a -> IO a
readStream s
  = do
    msg <- readStreamMsg s
    return (msg_Data msg)


readStreamMsg :: (Show a, Binary a) => Stream a -> IO (Message a)
readStreamMsg (Stream _ _ ch)
  = do
    debugM localLogger "PORT: readStreamMsg 1"
    res <- readChan ch
    debugM localLogger "PORT: readStreamMsg 2"
    return $ decodeMessage res


isEmptyStream :: Stream a -> IO Bool
isEmptyStream (Stream _ _ ch)
  = do
    isEmptyChan ch


--TODO think about this
tryStreamAction :: Stream a -> (Stream a -> IO (b)) -> IO (Maybe b)
tryStreamAction s f
  = do
    timeout 10 (f s)


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
    s <- newStream
    res <- f s
    closeStream s
    return res


-- -----------------------------------------------------------------------------
-- Stream-Server Operations  
-- -----------------------------------------------------------------------------


startStreamServer :: PortNumber -> PortNumber -> IO (Maybe (SocketId, ServerId))
startStreamServer actPo maxPo
  = do
    res <- Server.startSocket (streamDispatcher) actPo maxPo
    case res of
      Nothing ->
        return Nothing
      (Just (tid, hn, po)) ->
        return (Just (SocketId hn po, tid))
    

stopStreamServer :: ServerId -> IO ()
stopStreamServer sId 
  = do
    me <- myThreadId
    debugM localLogger $ "stopping server... with threadId: " ++ show sId ++ " - form threadId: " ++ show me
    throwDynTo sId me
    yield


streamDispatcher :: Handle -> HostName -> PortNumber -> IO ()
streamDispatcher hdl hn po
  = do
    debugM localLogger "streamDispatcher: getting message from handle"
    msg <- getMessage hdl
    debugM localLogger $ "streamDispatcher: Message: " ++ show msg
    debugM localLogger "streamDispatcher: writing message to channel"
    let name = getMessageReceiverName msg
    ch <- lookupStreamChannel name
    case ch of
      (Just c) -> 
         writeChannel c $ updateSender msg (SocketId hn po)
      _ -> return ()
    debugM localLogger "streamDispatcher: message written to channel"


putMessage :: Message B.ByteString -> Handle -> IO ()
putMessage msg hdl
  = do
    --putStrLn $ show msg
    --TODO better exception handling
    handle (\e -> errorM localLogger $ show e) $ do
      enc <- return (encode msg)
      --putStrLn $ show $ B.length enc
      --putStrLn $ show enc
      hPutStrLn hdl ((show $ B.length enc) ++ " ")
      B.hPut hdl enc


getMessage :: Handle -> IO (Message B.ByteString)
getMessage hdl
  = do
    --TODO better exception handling
    -- read the Package
    line <- hGetLine hdl
    --putStrLn line
    let pkg = words line -- \$ hGetLine hdl
    --putStrLn $ show pkg
    -- read the raw data 
    raw <- B.hGet hdl (read $ head pkg)
    --putStrLn $ show raw
    return (decode raw)



-- -----------------------------------------------------------------------------
-- Port Operations  
-- -----------------------------------------------------------------------------


-- | Creates a new Port, which is bound to a stream.
--   This port is internal, you automatically get an external port, if you
--   seriablize the port via the Binary-Class
newPort :: Stream a -> IO (Port a)
newPort s
  = do
    withMVar streamController $
      \sc ->
      do
      let scd = fromJust sc
      let i = s_StreamId s
      let n = s_Name s
      let so = scd_SocketId scd
      let si = scd_SiteId scd
      return (SinglePort i n so si)


isLocal :: SocketId -> SiteId -> IO Bool
isLocal so si
  = withMVar streamController $
      \sc ->
      do
      case sc of
        (Nothing) -> 
          return False
        (Just scd) ->
          do
          let so' = scd_SocketId scd
          let si' = scd_SiteId scd
          return (so == so' && si == si')
  

-- | Creates an external port from an internal one.
--   Used when seriablizing port via the binary package, so we can use the
--   port on external machines
{-
makePortExternal :: Port a -> Port a
makePortExternal (SinglePort (Stream i _ s _)) = ExternalPort i s
makePortExternal s@(ExternalPort _ _) = s
makePortExternal (BroadcastPort l) = BroadcastPort (map makePortExternal l)
-}

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
-- sendWithMaybeGeneric (InternalPort (Stream _ _ _ ch)) d rp
--   = do
sendWithMaybeGeneric (SinglePort _ name so@(SocketId hn po) site) d rp
  = do
    local <- isLocal so site
    if local
      then do
        msg <- newMessage Internal name d rp
        ch <- lookupStreamChannel name
        case ch of
          (Just c) ->
             writeChannel c $ encodeMessage $ updateSender msg (SocketId hn po)
          _ -> return ()
        return ()
      else do
        msg <- newMessage External name d rp
        Client.sendRequest (putMessage $ encodeMessage $ updateReceiver msg so) hn (PortNumber po)
        return ()
sendWithMaybeGeneric (BroadcastPort l) d rp
  = do
    sequence $ map (\p -> sendWithMaybeGeneric p d rp) l
    return ()

-- | Merges two ports, in into one.
--   The messages will be send to all connected streams. By this way we are
--   able to create broadcast-messages. If the two ports connect to the same
--   stream, two messages will be send
mergePorts :: Port a -> Port a -> Port a
mergePorts (BroadcastPort l1) (BroadcastPort l2) = BroadcastPort (l1 ++ l2)
mergePorts (BroadcastPort l) p = BroadcastPort (p:l)
mergePorts p (BroadcastPort l) = BroadcastPort (p:l)
mergePorts p1 p2 = BroadcastPort (p1:p2:[])


-- | Writes a port-description to a file.
--   Quite useful fpr sharing ports between programs
writePortToFile :: Port a -> FilePath -> IO ()
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
readPortFromFile :: FilePath -> IO (Port a)
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

        
printStreamController :: IO ()
printStreamController
  = withMVar streamController $
    \sc ->
    do
    putStrLn "StreamController:"
    case sc of
      (Nothing) ->
          putStrLn "not initialized"
      (Just scd) ->
         putStrLn $ show scd 