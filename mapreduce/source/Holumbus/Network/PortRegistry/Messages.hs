-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Network.PortRegistry.Messages
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.Network.PortRegistry.Messages
(
  PortRegistryRequestStream
, PortRegistryRequestPort

, PortRegistryResponseStream
, PortRegistryResponsePort

, PortRegistryRequestMessage(..)
, PortRegistryResponseMessage(..)
)
where

import           Data.Binary

import           Holumbus.Network.Port
import           Holumbus.Network.Core
import           Holumbus.Network.Messages


-- ----------------------------------------------------------------------------
-- Ports
-- ----------------------------------------------------------------------------


type PortRegistryRequestStream  = Stream PortRegistryRequestMessage
type PortRegistryRequestPort    = Port PortRegistryRequestMessage

type PortRegistryResponseStream = Stream PortRegistryResponseMessage
type PortRegistryResponsePort   = Port PortRegistryResponseMessage



-- ----------------------------------------------------------------------------
-- Messages
-- ----------------------------------------------------------------------------

data PortRegistryRequestMessage
  = PRReqRegister StreamName SocketId
  | PRReqUnregister StreamName
  | PRReqLookup StreamName
  | PRReqGetPorts
  | PRReqUnknown
  deriving (Show)

instance Binary PortRegistryRequestMessage where
  put (PRReqRegister sn soid) = putWord8 1 >> put sn >> put soid
  put (PRReqUnregister sn)    = putWord8 2 >> put sn
  put (PRReqLookup sn)        = putWord8 3 >> put sn
  put (PRReqGetPorts)         = putWord8 4
  put (PRReqUnknown)          = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> get >>= \sn -> get >>= \soid -> return (PRReqRegister sn soid)
        2 -> get >>= \sn -> return (PRReqUnregister sn)
        3 -> get >>= \sn -> return (PRReqLookup sn)
        4 -> return (PRReqGetPorts)
        _ -> return (PRReqUnknown)



data PortRegistryResponseMessage
  = PRRspSuccess
  | PRRspLookup (Maybe SocketId)
  | PRRspGetPorts [(String,SocketId)]
  | PRRspError String
  | PRRspUnknown
  deriving (Show)

instance Binary PortRegistryResponseMessage where
  put (PRRspSuccess)     = putWord8 1
  put (PRRspLookup soid) = putWord8 2 >> put soid
  put (PRRspGetPorts ls) = putWord8 3 >> put ls
  put (PRRspError e)     = putWord8 4 >> put e
  put (PRRspUnknown)     = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> return (PRRspSuccess)
        2 -> get >>= \soid -> return (PRRspLookup soid)
        3 -> get >>= \ls -> return (PRRspGetPorts ls)
        4 -> get >>= \e -> return (PRRspError e)
        _ -> return (PRRspUnknown)

instance RspMsg PortRegistryResponseMessage where
  isError (PRRspError _) = True
  isError _ = False
  
  getErrorMsg (PRRspError e) = e
  getErrorMsg _ = ""
  
  isUnknown (PRRspUnknown) = True
  isUnknown _ = False  
    
  mkErrorMsg e = PRRspError e