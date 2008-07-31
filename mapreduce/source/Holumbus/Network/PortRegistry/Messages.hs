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
  | PRRegUnknown
  deriving (Show)

instance Binary PortRegistryRequestMessage where
  put = undefined
  get = undefined



data PortRegistryResponseMessage
  = PRRspSuccess
  | PRRspLookup (Maybe SocketId)
  | PRRspGetPorts [(String,SocketId)]
  | PRRspError String
  | PRRspUnknown
  deriving (Show)

instance Binary PortRegistryResponseMessage where
  put = undefined
  get = undefined

instance RspMsg PortRegistryResponseMessage where
  isError (PRRspError _) = True
  isError _ = False
  
  getErrorMsg (PRRspError e) = e
  getErrorMsg _ = ""
  
  isUnknown (PRRspUnknown) = True
  isUnknown _ = False  
    
  mkErrorMsg e = PRRspError e