-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Network.PortRegistry
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.Network.PortRegistry
(
--  Type-Classes
  PortRegistry(..)
, UndefinedPortRegistry(..)
)
where

import Holumbus.Network.Core

-- ----------------------------------------------------------------------------
-- Type-Class
-- ----------------------------------------------------------------------------

class PortRegistry pr where

  registerPort :: String -> SocketId -> pr -> IO ()

  unregisterPort :: String -> pr -> IO ()

  lookupPort :: String -> pr -> IO (Maybe SocketId)

  getPorts :: pr -> IO [(String, SocketId)]
  
  
  
data UndefinedPortRegistry = UndefinedPortRegistry

instance PortRegistry UndefinedPortRegistry where

  registerPort _ _ _ = undefined
  unregisterPort _ _ = undefined
  lookupPort _ _     = undefined
  getPorts _         = undefined
