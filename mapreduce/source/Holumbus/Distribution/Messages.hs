-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Distribution.Messages
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.Distribution.Messages
(
-- * Datatypes
  WorkerId

-- * Request and Response Ports for the Master
, MasterRequestStream
, MasterRequestPort

, MasterResponseStream
, MasterResponsePort

-- * Request and Response Ports for the Worker
, WorkerRequestStream
, WorkerRequestPort

, WorkerResponseStream
, WorkerResponsePort

, decodeMasterResponsePort

, decodeWorkerResponsePort

-- * Messages to and from the Master
, MasterRequestMessage(..)
, MasterResponseMessage(..)

-- * Messages to and from the Worker
, WorkerRequestMessage(..)
, WorkerResponseMessage(..)

-- * request an response handling
, performPortAction -- reexport from Holumbus.Network.Messages
)
where


import           Data.Binary
import qualified Data.ByteString.Lazy as B

import qualified Holumbus.Network.Port as P
import qualified Holumbus.Network.Site as Site
import           Holumbus.Network.Messages
import           Holumbus.MapReduce.Types

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


type WorkerId = Integer



-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


-- Request and Response Ports for the Master
type MasterRequestStream  = P.Stream MasterRequestMessage
type MasterRequestPort    = P.Port MasterRequestMessage


type MasterResponseStream = P.Stream MasterResponseMessage
type MasterResponsePort   = P.Port MasterResponseMessage


-- Request and Response Ports for the Worker
type WorkerRequestStream  = P.Stream WorkerRequestMessage
type WorkerRequestPort    = P.Port WorkerRequestMessage


type WorkerResponseStream = P.Stream WorkerResponseMessage
type WorkerResponsePort   = P.Port WorkerResponseMessage


-- | parses something from a maybe bytestring, if Nothing, then Nothing
decodeMaybe :: (Binary a) => Maybe B.ByteString -> Maybe a
decodeMaybe Nothing = Nothing
decodeMaybe (Just b) = (Just $ decode b)


-- | parses a Master reply port from a bytestring 
decodeMasterResponsePort :: Maybe B.ByteString -> Maybe MasterResponsePort
decodeMasterResponsePort = decodeMaybe


-- | parses a Worker reply port from a bytestring
decodeWorkerResponsePort :: Maybe B.ByteString -> Maybe WorkerResponsePort
decodeWorkerResponsePort = decodeMaybe



-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


-- Messages to and from the Master
data MasterRequestMessage 
  = MReqRegister Site.SiteId WorkerRequestPort
  | MReqUnregister WorkerId
  | MReqAddJob JobInfo
  | MReqSingleStep
  | MReqTaskCompleted TaskData
  | MReqTaskError TaskData
  | MReqUnknown
  deriving (Show)


instance Binary MasterRequestMessage where
  put (MReqRegister s p)     = putWord8 1 >> put s >> put p
  put (MReqUnregister n)     = putWord8 2 >> put n
  put (MReqAddJob ji)        = putWord8 3 >> put ji
  put (MReqSingleStep)       = putWord8 4
  put (MReqTaskCompleted td) = putWord8 5 >> put td
  put (MReqTaskError td)     = putWord8 6 >> put td
  put (MReqUnknown)          = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> get >>= \s -> get >>= \p -> return (MReqRegister s p) 
        2 -> get >>= \n -> return (MReqUnregister n)
        3 -> get >>= \ji-> return (MReqAddJob ji)
        4 -> return (MReqSingleStep)
        5 -> get >>= \td -> return (MReqTaskCompleted td)
        6 -> get >>= \td -> return (MReqTaskError td) 
        _ -> return (MReqUnknown)



-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


data MasterResponseMessage 
  = MRspSuccess
  | MRspRegister WorkerId
  | MRspUnregister
  | MRspError String
  | MRspUnknown
  deriving (Show)


instance RspMsg MasterResponseMessage where
  isError (MRspError _) = True
  isError _ = False
  
  getErrorMsg (MRspError e) = e
  getErrorMsg _ = ""
  
  isUnknown (MRspUnknown) = True
  isUnknown _ = False  


instance Binary MasterResponseMessage where
  put (MRspSuccess)                       = putWord8 1
  put (MRspRegister n)                    = putWord8 2 >> put n 
  put (MRspUnregister)                    = putWord8 3
  put (MRspError e)                       = putWord8 4 >> put e
  put (MRspUnknown)                       = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> return (MRspSuccess)
        2 -> get >>= \n -> return (MRspRegister n) 
        3 -> return (MRspUnregister)
        4 -> get >>= \e -> return (MRspError e)
        _ -> return (MRspUnknown)




-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


-- Messages to and from the Worker
data WorkerRequestMessage
  = WReqStartTask TaskData
  | WReqUnknown
  deriving (Show)


instance Binary WorkerRequestMessage where
  put (WReqStartTask td) = putWord8 1 >> put td
  put (WReqUnknown) = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> get >>= \td -> return (WReqStartTask td)
        _ -> return (WReqUnknown)




-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


data WorkerResponseMessage
  = WRspSuccess
  | WRspError String 
  | WRspUnknown
  deriving (Show)


instance RspMsg WorkerResponseMessage where
  isError (WRspError _) = True
  isError _ = False
  
  getErrorMsg (WRspError e) = e
  getErrorMsg _ = ""
  
  isUnknown (WRspUnknown) = True
  isUnknown _ = False


instance Binary WorkerResponseMessage where
  put (WRspSuccess)          = putWord8 1
  put (WRspError e)          = putWord8 2 >> put e
  put (WRspUnknown)          = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> return (WRspSuccess)
        2 -> get >>= \e  -> return (WRspError e)
        _ -> return (WRspUnknown)
