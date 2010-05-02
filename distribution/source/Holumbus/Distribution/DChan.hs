-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Distribution.DChan
  Copyright  : Copyright (C) 2009 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  This module offers a distributed channel datatype (DChan).

  It is similar to Control.Concurrent.Chan, except that you can use it
  between multiple processes on different computers. You can access a
  DChan (reading and writing) from your local process as well as from
  another one.
-}

-- ----------------------------------------------------------------------------

module Holumbus.Distribution.DChan 
(
  -- * datatypes
    DChan
  
  -- * creating and closing channels
  , newDChan
  , newRemoteDChan
  , closeDChan
  
  -- * operations on a channel
  , writeDChan
  , readDChan
  , tryReadDChan
  , tryWaitReadDChan
  , isEmptyDChan
)
where

import           Control.Concurrent.Chan
import           Data.Binary
import qualified Data.ByteString.Lazy as B
import           System.IO
import           System.Log.Logger
import           System.Timeout

import           Holumbus.Distribution.DNode.Base

localLogger :: String
localLogger = "Holumbus.Distribution.DChan"


dChanType :: DResourceType
dChanType = mkDResourceType "DCHAN"

mkDChanEntry :: (Binary a) => DChanReference a -> DResourceEntry
mkDChanEntry d = DResourceEntry {
    dre_Dispatcher   = dispatchDChanRequest d 
  }


data DChanRequestMessage
  = DCMReqRead
  | DCMReqWrite B.ByteString
  | DCMReqIsEmpty
  deriving (Show)

instance Binary DChanRequestMessage where
  put(DCMReqRead)    = putWord8 1
  put(DCMReqWrite a) = putWord8 2 >> put a
  put(DCMReqIsEmpty) = putWord8 3
  get
    = do
      t <- getWord8
      case t of
        1 -> return (DCMReqRead)
        2 -> get >>= \a -> return (DCMReqWrite a)
        3 -> return (DCMReqIsEmpty)
        _ -> error "DChanRequestMessage: wrong encoding"

  
data DChanResponseMessage
  = DCMRspRead B.ByteString
  | DCMRspWrite
  | DCMRspIsEmpty Bool
  deriving (Show)


instance Binary DChanResponseMessage where
  put(DCMRspRead a)    = putWord8 1 >> put a
  put(DCMRspWrite)     = putWord8 2
  put(DCMRspIsEmpty b) = putWord8 3 >> put b
  get
    = do
      t <- getWord8
      case t of
        1 -> get >>= \a -> return (DCMRspRead a)
        2 -> return (DCMRspWrite)
        3 -> get >>= \b -> return (DCMRspIsEmpty b)
        _ -> error "DChanResponseMessage: wrong encoding"


dispatchDChanRequest :: (Binary a) => DChanReference a -> DNodeId -> Handle -> IO () 
dispatchDChanRequest dch _ hdl
  = do
    debugM localLogger "dispatcher: getting message from handle"
    raw <- getByteStringMessage hdl
    let msg = (decode raw)
    -- debugM localLogger $ "dispatcher: Message: " ++ show msg
    case msg of
      (DCMReqRead)    -> handleRead dch hdl
      (DCMReqWrite d) -> handleWrite dch (decode d) hdl
      (DCMReqIsEmpty) -> handleIsEmpty dch hdl

    
-- | The DChan datatype.
--   Notice that this datatype implements the Data.Binary typeclass.
--   That means that you can pass a DChan, so that another computer
--   can access the channel.
data DChan a
  = DChanLocal DResourceAddress (Chan a)
  | DChanRemote DResourceAddress

instance Binary (DChan a) where
  put(DChanLocal a _) = put a
  put(DChanRemote a)  = put a
  get = get >>= \a -> return (DChanRemote a)


data DChanReference a = DChanReference DResourceAddress (Chan a) 


-- | Creates a new DChan on the local computer. The first parameter
--   is the name of the Channel which could be used in other processes to
--   access this stream. If you leave it empty, a random Id will be created.
newDChan :: (Binary a) => String -> IO (DChan a)
newDChan s
  = do
    dra <- genLocalResourceAddress dChanType s
    c <- newChan
    let dch = (DChanLocal dra c)
        dcr = (DChanReference dra c)
        dce = (mkDChanEntry dcr)
    addLocalResource dra dce
    return dch
    
-- TODO merge this with newDChan?
-- | Creates a reference to a DChan which was created in a different
--   process.
--   The first parameter is the name of the resource and the second one
--   the name of the node.
newRemoteDChan :: String -> String -> IO (DChan a)
newRemoteDChan r n
  = do
    return $ DChanRemote dra
    where
    dra = mkDResourceAddress dChanType r n


-- | Closes a DChan object, could not be used anymore after this call.
closeDChan :: (DChan a) -> IO ()
closeDChan (DChanLocal dra _)
  = do
    delLocalResource dra
closeDChan (DChanRemote dra)
  = do
    delForeignResource dra



requestRead :: (Binary a) => Handle -> IO a
requestRead hdl
  = do
    putByteStringMessage (encode $ DCMReqRead) hdl
    raw <- getByteStringMessage hdl
    let rsp = (decode raw)
    case rsp of
      (DCMRspRead d)  -> return $ decode d
      _ -> error "DChan - requestRead: invalid response"
    

handleRead :: (Binary a) => DChanReference a -> Handle -> IO ()
handleRead (DChanReference _ ch) hdl
  = do
    a <- readChan ch
    putByteStringMessage (encode $ DCMRspRead $ encode a) hdl


requestWrite :: (Binary a) => a -> Handle -> IO ()
requestWrite a hdl
  = do 
    putByteStringMessage (encode $ DCMReqWrite $ encode a) hdl
    raw <- getByteStringMessage hdl
    let rsp = (decode raw)
    case rsp of
      (DCMRspWrite) -> return ()
      _ -> error "DChan - requestWrite: invalid response"
    

handleWrite :: DChanReference a -> a -> Handle -> IO ()
handleWrite (DChanReference _ ch) a hdl
  = do
    writeChan ch a
    putByteStringMessage (encode $ DCMRspWrite) hdl


requestIsEmpty :: Handle -> IO Bool
requestIsEmpty hdl
  = do
    putByteStringMessage (encode $ DCMReqIsEmpty) hdl
    raw <- getByteStringMessage hdl
    let rsp = (decode raw)
    case rsp of
      (DCMRspIsEmpty b) -> return b
      _ -> error "DChan - requestIsEmpty: invalid response"
    

handleIsEmpty :: DChanReference a -> Handle -> IO ()
handleIsEmpty (DChanReference _ ch) hdl
  = do
    b <- isEmptyChan ch
    putByteStringMessage (encode $ DCMRspIsEmpty b) hdl 


-- | Writes data to a DChan.
writeDChan :: (Binary a) => DChan a -> a -> IO ()
writeDChan (DChanLocal _ c) v
  = do
    writeChan c v
writeDChan (DChanRemote a) v
  = do
    unsafeAccessForeignResource a (requestWrite v)


-- | Reads data from a DChan, blocks if DChan is empty.
readDChan :: (Binary a) => DChan a -> IO a
readDChan (DChanLocal _ c)
  = do
    readChan c
readDChan (DChanRemote a)
  = do
    unsafeAccessForeignResource a requestRead


-- | Tries to read data from a DChan, if the DChan is empty,
--   the function return with Nothing.
tryReadDChan :: (Binary a) => DChan a -> IO (Maybe a)
tryReadDChan dc
  = do
    empty <- isEmptyDChan dc
    if (not empty) 
      then do timeout 1000 (readDChan dc)
      else do return Nothing


-- | Reads data from a DChan. If the channel is empty, it waits
--   for a given time (in microseconds) an returns immediately
--   when new data arrives, otherwise it return Nothing.
tryWaitReadDChan :: (Binary a) => DChan a -> Int -> IO (Maybe a) 
tryWaitReadDChan dc t = timeout t (readDChan dc)
    

-- | Tests, if a DChan is empty.
isEmptyDChan :: DChan a -> IO Bool
isEmptyDChan (DChanLocal _ c)
  = do
    isEmptyChan c
isEmptyDChan (DChanRemote a)
  = do
    unsafeAccessForeignResource a requestIsEmpty
