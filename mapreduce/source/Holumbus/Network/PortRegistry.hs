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

{-# OPTIONS -fglasgow-exts #-}
module Holumbus.Network.PortRegistry
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

class PortRegistry pr where

  registerPort :: String -> SocketId -> pr -> IO ()

  unregisterPort :: String -> pr -> IO ()

  lookupPort :: String -> pr -> IO (Maybe SocketId)

  getPorts :: pr -> IO [(String, SocketId)]
  



-- ----------------------------------------------------------------------------
-- GenericRegistry
-- ----------------------------------------------------------------------------
  
data GenericRegistry = forall r. (PortRegistry r) => GenericRegistry r


mkGenericRegistry :: (PortRegistry r) => r -> GenericRegistry
mkGenericRegistry = GenericRegistry


instance PortRegistry GenericRegistry where
  registerPort sn soid (GenericRegistry r) = registerPort sn soid r
  unregisterPort sn (GenericRegistry r) = unregisterPort sn r
  lookupPort sn (GenericRegistry r) = lookupPort sn r
  getPorts (GenericRegistry r) = getPorts r
