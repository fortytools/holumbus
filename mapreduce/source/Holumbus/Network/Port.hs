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

module Holumbus.Network.Port
(
-- * Constants
  time1
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

-- * Stream-Operations
, newStream
, newStreamOnPort
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
)
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Binary
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.Time
import Network
import System.IO
import System.Timeout

import Holumbus.Network.Site
import qualified Holumbus.Network.Server as Server
import qualified Holumbus.Network.Client as Client



-- -----------------------------------------------------------------------------
-- Constants
-- -----------------------------------------------------------------------------


-- | one second
time1 :: Int
time1 = 1000000


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
  deriving (Show)


instance Binary (SocketId) where
  put (SocketId hn po)
    = put hn >> (put . toInteger) po
  get
    = do
      hn <- get
      poInt <- get
      return (SocketId hn (fromInteger poInt))


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
    msg_type         :: ! MessageType          -- ^ the message-type
  , msg_data         :: ! a                    -- ^ the data  
  , msg_generic      :: ! (Maybe B.ByteString)     -- ^ some generic data -- could be another port
  , msg_receiver     :: ! (Maybe SocketId)     -- ^ socket to which the message is send (DEBUG)
  , msg_sender       :: ! (Maybe SocketId)     -- ^ socket from which the message was send (DEBUG)
  , msg_send_time    :: ! UTCTime              -- ^ timestamp from the sender (DEBUG)
  , msg_receive_time :: ! UTCTime              -- ^ timestamp from the receiver (DEBUG)
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
  put (Message t d g r s t1 t2)
    = do
      put t
      put d
      put g
      put r
      put s
      put $ show t1
      put $ show t2
  get
    = do
      typ <- get
      dat <- get
      gen <- get
      rec <- get
      sen <- get
      t1Str <- get
      t2Str <- get
      return (Message typ dat gen rec sen (read t1Str) (read t2Str))




-- -----------------------------------------------------------------------------
-- Stream-Datatype
-- -----------------------------------------------------------------------------


-- | The threadId of the server which accepts incomming messages for a stream 
type ServerId = ThreadId


-- | The stream datatype
data Stream a = Stream {
    s_sockedId :: SocketId           -- ^ socket-descriptor of the stream server  
  , s_serverId :: ServerId           -- ^ threadid of the stream server
  , s_siteId   :: SiteId             -- ^ SiteId (hostname and pid)
  , s_chan     :: (Chan (Message a)) -- ^ internal message queue
  }

instance Show (Stream a) where
  show (Stream i server site _)
    = "(Stream - ID: " ++
      show i ++
      " - ServerThreadID: " ++
      show server ++
      " - SiteID: " ++
      show site ++
      " )"



-- -----------------------------------------------------------------------------
-- Port-Datatype
-- -----------------------------------------------------------------------------      


data Port a
  = InternalPort (Stream a)       -- ^ internal Port
  | ExternalPort SocketId SiteId  -- ^ external Port
  | BroadcastPort [Port a]        -- ^ broadcast Port


instance Show (Port a) where
  show (InternalPort s)
    = "(InternalPort - Stream: " ++ show s ++ " )"
  show (ExternalPort i s)
    = "(ExternalPort - ID: " ++ show i ++ " - SiteID: " ++ show s ++ " )"
  show (BroadcastPort l)
    = "(BroadcastPort - " ++ (concat . map show) l ++ " )" 


instance Binary (Port a) where
  put s@(InternalPort _) 
    = put $ makePortExternal s    
  put (ExternalPort soId siteId)
    = do
      putWord8 1
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
          soid <- get
          siteId <- get
          return (ExternalPort soid siteId)
        _ ->
          do
          lst <- get
          return (BroadcastPort lst)




-- -----------------------------------------------------------------------------
-- Message-Operations
-- -----------------------------------------------------------------------------

        
newMessage :: (Show a, Binary a) => MessageType -> a -> Maybe B.ByteString -> IO (Message a)
newMessage t d g
  = do
    time <- getCurrentTime
    return (Message t d g Nothing Nothing time time)


getMessageType :: (Show a, Binary a) => Message a -> MessageType
getMessageType = msg_type


getMessageData :: (Show a, Binary a) => Message a -> a
getMessageData = msg_data


getGenericData :: (Show a, Binary a) => Message a -> (Maybe B.ByteString)
getGenericData = msg_generic

      
updateReceiveTime :: (Show a, Binary a) => Message a -> IO (Message a)
updateReceiveTime msg 
  = do
    time <- getCurrentTime
    return (msg {msg_receive_time = time})

    
updateReceiver :: (Show a, Binary a) => Message a -> SocketId -> Message a
updateReceiver msg soId = msg { msg_receiver = Just soId }


updateSender :: (Show a, Binary a) => Message a -> SocketId -> Message a
updateSender msg soId = msg { msg_sender = Just soId } 

-- TODO getter/setter
      
      


-- -----------------------------------------------------------------------------
-- Stream Operations  
-- -----------------------------------------------------------------------------


defaultPort :: PortNumber
defaultPort = 9000


maxPort :: PortNumber
maxPort = 40000


-- | Create a new stream.
--   The stream is bound to a random port.
newStream :: (Show a, Binary a) => IO (Stream a)
newStream 
  = do
    ch <- newChan
    s <- (startStreamServer defaultPort maxPort ch)
    let (i, server) = fromJust s
    siteId <- getSiteId
    putStrLn $ "newServer with threadId: " ++ show server 
    return (Stream i server siteId ch)


-- | Create a new stream.
--   The stream is bound to the port with the specified portnumber
newStreamOnPort :: (Show a, Binary a) => PortNumber -> IO (Stream a)
newStreamOnPort p
  = do
    ch <- newChan
    s <- startStreamServerOnPort p ch
    let (i, server) = fromJust s
    siteId <- getSiteId
    putStrLn $ "newServer with threadId: " ++ show server
    return (Stream i server siteId ch)


-- | Close a stream
--   Only the the socket is closed, if you keep the stream, you can internally
--   send data to it
-- TODO: think about this
closeStream :: (Binary a) => Stream a -> IO ()
closeStream (Stream _ server _ _)
  = do
    stopStreamServer server


-- | Writes a message to the channel of the stream.
--   This function is not public, so you have to write to a stream throug its
--   port
writeChannel :: (Show a, Binary a) => Chan (Message a) -> Message a -> IO ()
writeChannel ch msg 
  = do
    newMsg <- updateReceiveTime msg
    writeChan ch newMsg


readStream :: (Show a, Binary a) => Stream a -> IO a
readStream s
  = do
    msg <- readStreamMsg s
    --putStrLn $ show msg
    return (msg_data msg)


readStreamMsg :: (Show a, Binary a) => Stream a -> IO (Message a)
readStreamMsg (Stream _ _ _ ch)
  = do
    putStrLn "PORT: readStreamMsg 1"
    res <- readChan ch
    putStrLn "PORT: readStreamMsg 2"
    return res


isEmptyStream :: Stream a -> IO Bool
isEmptyStream (Stream _ _ _ ch)
  = do
    isEmptyChan ch
    

--TODO think about this
tryStreamAction :: Stream a -> (Stream a -> IO (b)) -> IO (Maybe b)
tryStreamAction s f
  = do
    timeout 10 (f s)
{-  = do
    block $
      do
      e <- isEmptyStream s
      if e
        then do
          return Nothing
        else do
          d <- f s
          return (Just d)
-}


tryWaitStreamAction :: Stream a -> (Stream a -> IO (b)) -> Int -> IO (Maybe b)
tryWaitStreamAction s f t
  = do
    putStrLn "waiting..."
    r <- timeout t (f s)
    putStrLn "...finished"
    putStrLn $ maybe "nothing received" (\_ -> "value found") r
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

{-
    putStrLn "new tryWaitReadStream"
    threadDelay t
    b <- isEmptyStream s
    case b of
      True ->
        do
        return Nothing
      _ -> 
        do
        d <- readStream s
        return (Just d)
-}
    
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


startStreamServerOnPort :: (Show a, Binary a) => PortNumber -> Chan (Message a) -> IO (Maybe (SocketId, ServerId))
startStreamServerOnPort po ch = startStreamServer po po ch


startStreamServer :: (Show a, Binary a) => PortNumber -> PortNumber -> Chan (Message a) -> IO (Maybe (SocketId, ServerId))
startStreamServer actPo maxPo ch 
  = do
    res <- Server.startSocket (streamDispatcher ch) actPo maxPo
    case res of
      Nothing ->
        return Nothing
      (Just (tid, hn, po)) ->
        return (Just (SocketId hn po, tid))
    --servId <- forkIO $ Server.listenForRequests (streamDispatcher ch) (PortNumber po)
    

stopStreamServer :: ServerId -> IO ()
stopStreamServer sId 
  = do
    me <- myThreadId
    putStrLn $ "stopping server... with threadId: " ++ show sId ++ " - form threadId: " ++ show me
    throwDynTo sId me
    yield


streamDispatcher :: (Show a, Binary a) => Chan (Message a) -> Handle -> HostName -> PortNumber -> IO ()
streamDispatcher ch hdl hn po
  = do
    putStrLn "PORT: getting message from handle" 
    msg <- getMessage hdl
    putStrLn "PORT: writing message to channel"
    writeChannel ch $ updateSender msg (SocketId hn po)
    putStrLn "PORT: message written to channel"


putMessage :: (Show a, Binary a) => a -> Handle -> IO ()
putMessage msg hdl
  = do
    --putStrLn $ show msg
    --TODO better exception handling
    handle (\e -> putStrLn $ show e) $ do
      enc <- return (encode msg)
      --putStrLn $ show $ B.length enc
      --putStrLn $ show enc
      hPutStrLn hdl ((show $ B.length enc) ++ " ")
      B.hPut hdl enc


getMessage :: (Binary a) => Handle -> IO (a)
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
    return (InternalPort s) 


-- | Creates an external port from an internal one.
--   Used when seriablizing port via the binary package, so we can use the
--   port on external machines
makePortExternal :: Port a -> Port a
makePortExternal (InternalPort (Stream i _ s _)) = ExternalPort i s
makePortExternal s@(ExternalPort _ _) = s
makePortExternal (BroadcastPort l) = BroadcastPort (map makePortExternal l)


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
sendWithMaybeGeneric (InternalPort (Stream _ _ _ ch)) d rp
  = do
    msg <-newMessage Internal d rp
    writeChannel ch msg
sendWithMaybeGeneric (ExternalPort rec@(SocketId hn po) _) d rp
  = do
    msg <- newMessage External d rp
    Client.sendRequest (putMessage $ updateReceiver msg rec) hn (PortNumber po)
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
