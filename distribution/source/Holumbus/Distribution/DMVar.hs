-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Distribution.DMVar 
  Copyright  : Copyright (C) 2009 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
-}

-- ----------------------------------------------------------------------------

module Holumbus.Distribution.DMVar
(
    DMVar

  , newDMVar
  , newEmptyDMVar
  , newRemoteDMVar
  , closeDMVar

  , readDMVar
  , takeDMVar
  , putDMVar
)
where

import           Prelude hiding (catch)

import           Control.Concurrent.MVar
--import           Data.Binary
import           Holumbus.Common.MRBinary
import qualified Data.ByteString.Lazy as B
import           System.IO
import           System.Log.Logger

import           Holumbus.Distribution.DNode.Base


localLogger :: String
localLogger = "Holumbus.Distribution.DMVar"

dMVarType :: DRessourceType
dMVarType = mkDRessourceType "DMVAR"

mkDMVarEntry :: (Binary a) => DMVarReference a -> DRessourceEntry
mkDMVarEntry d = DRessourceEntry {
    dre_Dispatcher   = dispatchDMVarRequest d 
  }


data DMVarRequestMessage
  = DVMReqRead
  | DVMReqTake
  | DVMReqPut B.ByteString
  deriving (Show)
    
instance Binary DMVarRequestMessage where
  put(DVMReqRead)  = putWord8 1
  put(DVMReqTake)  = putWord8 2
  put(DVMReqPut b) = putWord8 3 >> put b
  get
    = do
      t <- getWord8
      case t of
        1 -> return (DVMReqRead)
        2 -> return (DVMReqTake)
        3 -> get >>= \b -> return (DVMReqPut b)
        _ -> error "DMVarRequestMessage: wrong encoding"


data DMVarResponseMessage
  = DVMRspRead B.ByteString
  | DVMRspTake B.ByteString
  | DVMRspPut
  deriving (Show)

instance Binary DMVarResponseMessage where
  put(DVMRspRead b) = putWord8 1 >> put b
  put(DVMRspTake b) = putWord8 2 >> put b
  put(DVMRspPut)    = putWord8 3
  get
    = do
      t <- getWord8
      case t of
        1 -> get >>= \b -> return (DVMRspRead b)
        2 -> get >>= \b -> return (DVMRspTake b)
        3 -> return (DVMRspPut)
        _ -> error "DMVarResponseMessage: wrong encoding"


dispatchDMVarRequest :: (Binary a) => DMVarReference a -> DNodeId -> Handle -> IO () 
dispatchDMVarRequest dch dna hdl
  = do
    debugM localLogger "dispatcher: getting message from handle"
    raw <- getByteStringMessage hdl
    let msg = (decode raw)
    debugM localLogger $ "dispatcher: Message: " ++ show msg
    case msg of
      (DVMReqRead)  -> handleRead dch hdl
      (DVMReqTake)  -> handleTake dch dna hdl
      (DVMReqPut b) -> handlePut dch (decode b) hdl



data DMVar a
  = DMVarLocal DRessourceAddress (MVar a) (MVar (a, Maybe DHandlerId))
  | DMVarRemote DRessourceAddress

instance Binary (DMVar a) where
  put(DMVarLocal dra _ _) = put dra
  put(DMVarRemote dra)    = put dra
  get = get >>= \dra -> return (DMVarRemote dra)

data DMVarReference a = DMVarReference DRessourceAddress (MVar a) (MVar (a, Maybe DHandlerId))
  

newDMVar :: (Binary a) => String -> a -> IO (DMVar a)
newDMVar s d
  = do
    dra <- genLocalRessourceAddress dMVarType s
    v <- newMVar d
    o <- newEmptyMVar
    let dmv = (DMVarLocal dra v o)
        dvr = (DMVarReference dra v o)
        dve = (mkDMVarEntry dvr)
    addLocalRessource dra dve
    return dmv


newEmptyDMVar :: (Binary a) => String -> IO (DMVar a)
newEmptyDMVar s
  = do
    dra <- genLocalRessourceAddress dMVarType s
    v <- newEmptyMVar
    o <- newMVar (undefined, Nothing)
    let dmv = (DMVarLocal dra v o)
        dvr = (DMVarReference dra v o)
        dve = (mkDMVarEntry dvr)
    addLocalRessource dra dve
    return dmv


newRemoteDMVar :: String -> String -> IO (DMVar a)
newRemoteDMVar r n
  = do
    return $ DMVarRemote dra
    where
    dra = mkDRessourceAddress dMVarType r n


closeDMVar :: (DMVar a) -> IO ()
closeDMVar (DMVarLocal dra _ _)
  = do
    delLocalRessource dra
closeDMVar (DMVarRemote dra)
  = do
    delForeignRessource dra



requestRead :: (Binary a) => Handle -> IO a
requestRead hdl
  = do
    putByteStringMessage (encode $ DVMReqRead) hdl
    raw <- getByteStringMessage hdl
    let rsp = (decode raw)
    case rsp of
      (DVMRspRead d) -> return $ decode d
      _ -> error "DMVar - requestRead: invalid response"


handleRead :: (Binary a) => DMVarReference a -> Handle -> IO ()
handleRead (DMVarReference _ v _) hdl
  = do
    a <- readMVar v
    putByteStringMessage (encode $ DVMRspRead $ encode a) hdl



requestTake :: (Binary a) => Handle -> IO a
requestTake hdl
  = do
    putByteStringMessage (encode $ DVMReqTake) hdl
    raw <- getByteStringMessage hdl
    let rsp = (decode raw)
    case rsp of
      (DVMRspTake d) -> return $ decode d
      _ -> error "DMVar - requestTake: invalid response"


handleTake :: (Binary a) => DMVarReference a -> DNodeId -> Handle -> IO ()
handleTake r@(DMVarReference _ v o) dni hdl
  = do
    debugM localLogger $ "handleTake: 1"
    a <- takeMVar v
    debugM localLogger $ "handleTake: 2"
    -- install handler and save backup
    mbDhi <- addForeignDNodeHandler dni (handleErrorTake r)
    debugM localLogger $ "handleTake: 3"
    putMVar o (a, mbDhi)
    debugM localLogger $ "handleTake: 4"
    putByteStringMessage (encode $ DVMRspTake $ encode a) hdl
    debugM localLogger $ "handleTake: 5"
    

handleErrorTake :: (Binary a) => DMVarReference a -> IO ()
handleErrorTake (DMVarReference _ v o)
  = do
    debugM localLogger $ "handleErrorTake: 1"
    (a,_ ) <- takeMVar o
    -- no need to kill handler... it's only called once
    -- case mbDhi of
    --  (Just dhi) -> delForeignHandler dhi
    --  (Nothing)  -> return ()
    debugM localLogger $ "handleErrorTake: 2"
    putMVar v a


requestPut :: (Binary a) => a -> Handle -> IO ()
requestPut d hdl
  = do
    putByteStringMessage (encode $ DVMReqPut $ encode d) hdl
    raw <- getByteStringMessage hdl
    let rsp = (decode raw)
    case rsp of
      (DVMRspPut) -> return ()
      _ -> error "DMVar - requestWrite: invalid response"


handlePut :: (Binary a) => DMVarReference a -> a -> Handle -> IO ()
handlePut (DMVarReference _ v o) a hdl
  = do
    -- delete backup and kill handler 
    (_,mbDhi) <- takeMVar o
    case mbDhi of
      (Just dhi) -> delForeignHandler dhi
      (Nothing)  -> return ()
    putMVar v a
    putByteStringMessage (encode $ DVMRspPut) hdl



readDMVar :: (Binary a) => DMVar a -> IO a
readDMVar (DMVarLocal _ v _)
  = do
    readMVar v
readDMVar (DMVarRemote a)
  = do
    unsafeAccessForeignRessource a requestRead


takeDMVar :: (Binary a) => DMVar a -> IO a
takeDMVar (DMVarLocal _ v o)
  = do
    a <- takeMVar v
    putMVar o (a, Nothing)
    return a
takeDMVar (DMVarRemote a)
  = do
    unsafeAccessForeignRessource a requestTake


putDMVar :: (Binary a) => DMVar a -> a -> IO ()  
putDMVar (DMVarLocal _ v o) d
  = do
    (_,mbDhi) <- takeMVar o
    case mbDhi of
      (Just dhi) -> delForeignHandler dhi
      (Nothing)  -> return ()
    putMVar v d
putDMVar (DMVarRemote a) d
  = do
    unsafeAccessForeignRessource a (requestPut d)

