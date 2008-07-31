
module Holumbus.Network.PortRegistry
(
--  Type-Classes
  PortRegistry(..)
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