-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Network.PortRegistry.PortRegistryPort
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.Network.PortRegistry.PortRegistryPort
(
-- * Datatypes
  PortRegistryPort
  
-- * Creation
, newPortRegistryPort
, newPortRegistryFromData
, newPortRegistryFromXmlFile 

-- * reexport
, setPortRegistry 
)
where

import System.Log.Logger

import Holumbus.Common.FileHandling
import Holumbus.Network.Port
import Holumbus.Network.Messages
import Holumbus.Network.PortRegistry
import Holumbus.Network.PortRegistry.Messages


localLogger :: String
localLogger = "Holumbus.Network.PortRegistry.PortRegistryPort"


-- ----------------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------------
  
data PortRegistryPort = PortRegistryPort PortRegistryRequestPort
  deriving (Show)




-- ----------------------------------------------------------------------------
--  Creation
-- ----------------------------------------------------------------------------

-- | Creates a new NodePort.
newPortRegistryPort :: PortRegistryRequestPort -> PortRegistryPort
newPortRegistryPort p = PortRegistryPort p


newPortRegistryFromData :: StreamName -> SocketId -> IO PortRegistryPort
newPortRegistryFromData sn soid
  = do
    p <- newPort sn (Just soid)
    return $ newPortRegistryPort p


newPortRegistryFromXmlFile :: FilePath -> IO PortRegistryPort
newPortRegistryFromXmlFile fp
  = do
    p <- loadFromXmlFile fp
    return $ newPortRegistryPort p



-- ----------------------------------------------------------------------------
-- Typeclass instanciation for PortRegistryPort
-- ----------------------------------------------------------------------------

instance PortRegistry PortRegistryPort where
    
  registerPort sn soid (PortRegistryPort p)
    = do
      debugM localLogger "registerPort start"
      r <- withStream $
        \s -> performPortAction p s time30 (PRReqRegister sn soid) $
          \rsp ->
          do
          case rsp of
            (PRRspSuccess) -> return (Just $ ())
            _ -> return Nothing
      debugM localLogger "registerPort end"
      return r


  unregisterPort sn (PortRegistryPort p)
    = do
      debugM localLogger "unregisterPort start"
      r <- withStream $
        \s -> performPortAction p s time30 (PRReqUnregister sn) $
          \rsp ->
          do
          case rsp of
            (PRRspSuccess) -> return (Just $ ())
            _ -> return Nothing
      debugM localLogger "unregisterPort end"
      return r
  
  lookupPort sn (PortRegistryPort p)
    = do
      debugM localLogger "lookupPort start"
      r <- withStream $
        \s -> performPortAction p s time30 (PRReqLookup sn) $
          \rsp ->
          do
          case rsp of
            (PRRspLookup soid) -> return (Just $ soid)
            _ -> return Nothing
      debugM localLogger "lookupPort end"
      return r
  

  getPorts (PortRegistryPort p)
    = do
      debugM localLogger "getPorts start"
      r <- withStream $
        \s -> performPortAction p s time30 (PRReqGetPorts) $
          \rsp ->
          do
          case rsp of
            (PRRspGetPorts ls) -> return (Just $ ls)
            _ -> return Nothing
      debugM localLogger "getPorts end"
      return r
  