-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Distribution.DStreamPort
  Copyright  : Copyright (C) 2009 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
-}

-- ----------------------------------------------------------------------------

module Holumbus.Distribution.DStreamPort
(
    DStream
  , DPort
  , StreamPortMessage(..)
  
  , newDStream
  , closeDStream
  , isEmptyDStream
  
  , receive
  , receiveMsg
  , tryReceive
  , tryReceiveMsg
  , tryWaitReceive
  , tryWaitReceiveMsg
  , withStream
    
  , newDPortFromStream
  , newDPort
  , send
  , sendWithGeneric
  , sendWithMaybeGeneric
)
where

import           Data.Binary
import qualified Data.ByteString.Lazy as B
import           System.Log.Logger

import           Holumbus.Distribution.DChan


localLogger :: String
localLogger = "Holumbus.Distribution.DStreamPort"

-- ----------------------------------------------------------------------------
-- | Message Datatype.
--   We are sending additional information, to do debugging
data (Binary a) => StreamPortMessage a = StreamPortMessage {
    spm_Data           :: ! a                    -- ^ the data  
  , spm_Generic        :: ! (Maybe B.ByteString) -- ^ some generic data -- could be another port
  }

instance (Binary a) => Binary (StreamPortMessage a) where
  put(StreamPortMessage d g) = put d >> put g
  get = get >>= \d -> get >>= \g -> return $ (StreamPortMessage d g)


-- ----------------------------------------------------------------------------
newtype DStream a = DStream (DChan (StreamPortMessage a))


newDStream :: (Binary a) => String -> IO (DStream a) 
newDStream s
  = do
    dc <- newDChan s
    return (DStream dc)


closeDStream :: DStream a -> IO ()
closeDStream (DStream dc) = closeDChan dc


isEmptyDStream :: DStream a -> IO Bool
isEmptyDStream (DStream dc) = isEmptyDChan dc


getMaybeMsgData :: (Binary a) => Maybe (StreamPortMessage a) -> Maybe a
getMaybeMsgData (Nothing)  = Nothing
getMaybeMsgData (Just msg) = Just (spm_Data msg)

-- | Reads the data packet of the next message from a stream.
--   If stream is empty, this function will block until a new message arrives.
receive :: (Binary a) => DStream a -> IO a
receive s = (receiveMsg s) >>= \msg -> return (spm_Data msg)


-- | Reads the next message from a stream (data packet + message header).
--   If stream is empty, this function will block until a new message arrives.
receiveMsg :: (Binary a) => DStream a -> IO (StreamPortMessage a)
receiveMsg (DStream dc) = readDChan dc


-- | Reads the data packet of the next message from a stream.
--   If stream is empty, this function will immediately return with Nothing.
tryReceive :: (Binary a) => DStream a -> IO (Maybe a)
tryReceive s = tryReceiveMsg s >>= \msg -> return $ getMaybeMsgData msg


-- | Reads the next message from a stream (data packet + message header).
--   If stream is empty, this function will immediately return with Nothing.
tryReceiveMsg :: (Binary a) => DStream a -> IO (Maybe (StreamPortMessage a))
tryReceiveMsg (DStream dc) = tryReadDChan dc


-- | Reads the data packet of the next message from a stream.
--   If stream is empty, this function will wait for new messages until the 
--   time is up and if no message has arrived, return with Nothing.
tryWaitReceive :: (Binary a) => DStream a -> Int -> IO (Maybe a)
tryWaitReceive s t = (tryWaitReceiveMsg s t) >>= \msg -> return $ getMaybeMsgData msg


-- | Reads the next message from a stream (data packet + message header).
--   If stream is empty, this function will wait for new messages until the 
--   time is up and if no message has arrived, return with Nothing.
tryWaitReceiveMsg :: (Binary a) => DStream a -> Int -> IO (Maybe (StreamPortMessage a))
tryWaitReceiveMsg (DStream dc) t = tryWaitReadDChan dc t

    
-- | Encapsulates a stream.
--   A new stream is created, then some user-action is done an after that the
--   stream is closed. 
withStream :: (Binary a) => (DStream a -> IO b) -> IO b
withStream f
  = do
    debugM localLogger "withStream: creating new stream"
    s <- newDStream ""
    debugM localLogger "withStream: new stream created"
    res <- f s
    closeDStream s
    return res

   
-- ----------------------------------------------------------------------------
newtype DPort a = DPort (DChan (StreamPortMessage a))

instance (Binary a) => Binary (DPort a) where
  put(DPort dc) = put dc
  get = get >>= \dc -> return $ (DPort dc)
    
    
-- | Creates a new Port, which is bound to a stream.
newDPortFromStream :: DStream a -> IO (DPort a)
newDPortFromStream (DStream dc) = return (DPort dc)


-- | Creates a new port from a streamname and its socketId.
newDPort :: String -> String -> IO (DPort a)
newDPort r n = newRemoteDChan r n >>= \dc -> return (DPort dc)
    
   
-- | Send data to the stream of the port.
--   The data is send via network, if the stream is located on an external
--   processor
send :: (Binary a) => DPort a -> a -> IO ()
send p d = sendWithMaybeGeneric p d Nothing


-- | Like "send", but here we can give some generic data (e.g. a port for reply 
--   messages).
sendWithGeneric :: (Binary a) => DPort a -> a -> B.ByteString -> IO ()
sendWithGeneric p d rp = sendWithMaybeGeneric p d (Just rp) 


-- | Like "sendWithGeneric", but the generic data is optional
sendWithMaybeGeneric :: (Binary a) => DPort a -> a -> Maybe B.ByteString -> IO ()
sendWithMaybeGeneric (DPort dc) d rp = writeDChan dc (StreamPortMessage d rp)
