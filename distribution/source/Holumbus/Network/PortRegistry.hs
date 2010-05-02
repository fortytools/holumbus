-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Network.PortRegistry
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


  The typeclass for the PortRegistry. Contains all PortRegistry functions which
  can accessed remotely by the clients.
  
-}
-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts #-}
module Holumbus.Network.PortRegistry
{-# DEPRECATED "this module will be remove in the next release, please use the packages from Holumbus.Distribution.*" #-}
    (
      -- * Type-Classes
      PortRegistry(..)

      -- * GenericRegistry
    , GenericRegistry
    , mkGenericRegistry
    )
where

import Holumbus.Network.Core

-- ----------------------------------------------------------------------------
-- Type-Class
-- ----------------------------------------------------------------------------

-- | The Interface of the PortRegistry.
class PortRegistry pr where

  -- | Creates a new port entry in the registry.
  registerPort :: String -> SocketId -> pr -> IO ()

  -- | Deletes an entry from the registry.
  unregisterPort :: String -> pr -> IO ()

  -- | Get the socket of the port by its name.
  lookupPort :: String -> pr -> IO (Maybe SocketId)

  -- | Get a list of all registered ports.
  getPorts :: pr -> IO [(String, SocketId)]
  



-- ----------------------------------------------------------------------------
-- GenericRegistry
-- ----------------------------------------------------------------------------
  
-- | The generic registry object.
--   This is a wrapper around an PortRegistryData or PortRegistryPort object.
--   With this additional indirection, we eliminate the distinction between
--   the port or the data object in the datatypes using the PortRegistry.
--   Therefore it is easier to get access to the registry and the code gets 
--   more readable.
--   This might be a good example of hiding network-access. To the caller it
--   makes no difference, if the PortRegistry is in the same address space of
--   on another computer in the network.

data GenericRegistry = forall r. (PortRegistry r) => GenericRegistry r


-- | Creates a new generic PortRegistry.
mkGenericRegistry :: (PortRegistry r) => r -> GenericRegistry
mkGenericRegistry = GenericRegistry


instance PortRegistry GenericRegistry where
  registerPort sn soid (GenericRegistry r) = registerPort sn soid r
  unregisterPort sn (GenericRegistry r) = unregisterPort sn r
  lookupPort sn (GenericRegistry r) = lookupPort sn r
  getPorts (GenericRegistry r) = getPorts r
