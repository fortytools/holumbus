-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Network.PortRegistry.PortRegistryData
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


  This module contains the main datatype for the PortRegistry.
  
-}
-- ----------------------------------------------------------------------------

module Holumbus.Network.PortRegistry.PortRegistryData
{-# DEPRECATED "this module will be remove in the next release, please use the packages from Holumbus.Distribution.*" #-}
(
-- * Datatypes
  PortRegistryData

-- * Creation and Destruction
, newPortRegistryData
, closePortRegistryData

, getPortRegistryRequestPort

-- * reexport
, setPortRegistry 
)
where


import           Control.Concurrent
import qualified Data.Map as Map
import           Network
import           System.Log.Logger

import           Holumbus.Common.Threading
import           Holumbus.Network.Port
import           Holumbus.Network.Messages
import           Holumbus.Network.PortRegistry
import           Holumbus.Network.PortRegistry.Messages


localLogger :: String
localLogger = "Holumbus.Network.PortRegistry.PortRegistryData"



-- ----------------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------------

-- | The data needed by the PortRegistry
data PortRegistryData = PortRegistryData {
    prd_ServerThreadId :: Thread                             -- ^ The thread-data of the message-dispatcher thread.
  , prd_OwnStream      :: PortRegistryRequestStream          -- ^ The Stream for all incomming messages.
  , prd_SocketMap      :: MVar (Map.Map StreamName SocketId) -- ^ The map storing the port-data (the real registry).
  }


-- ----------------------------------------------------------------------------
-- Creation and Destruction
-- ----------------------------------------------------------------------------


-- | Creates a new PortRegistry.
newPortRegistryData :: StreamName -> Maybe PortNumber -> IO PortRegistryData
newPortRegistryData sn pn
  = do
    st      <- (newStream STLocal (Just sn) pn)::IO PortRegistryRequestStream
    mMVar   <- newMVar Map.empty
    sMVar   <- newThread            
    let prd = PortRegistryData sMVar st mMVar
    startRequestDispatcher sMVar st (dispatch prd)
    return prd


-- | Closes the PortRegistry with its streams and threads.
closePortRegistryData :: PortRegistryData -> IO ()
closePortRegistryData prd
  = do
    stopRequestDispatcher (prd_ServerThreadId prd)
    closeStream (prd_OwnStream prd)
    return ()


-- | Get the RequestPort of the PortRegistry.
--   It can be used to give access to the PortRegistry, eg. you can serialize
--   this information and transfer it over the network to grant access to the
--   clients.
getPortRegistryRequestPort :: PortRegistryData -> IO PortRegistryRequestPort
getPortRegistryRequestPort prd = newPortFromStream (prd_OwnStream prd)


-- | The main dispatch-function. It handles the incomming messages and reacts.
dispatch 
  :: PortRegistryData 
  -> PortRegistryRequestMessage 
  -> PortRegistryResponsePort
  -> IO ()
dispatch prd msg replyPort
  = do
    case msg of
      (PRReqRegister sn soid) ->
        do
        handleRequest replyPort (registerPort sn soid prd) (\_ -> PRRspSuccess)
        return ()
      (PRReqUnregister sn) ->      
        do
        handleRequest replyPort (unregisterPort sn prd) (\_ -> PRRspSuccess)
        return ()
      (PRReqLookup sn) ->
        do
        handleRequest replyPort (lookupPort sn prd) (\soid -> PRRspLookup soid)
        return ()
      (PRReqGetPorts) ->
        do
        handleRequest replyPort (getPorts prd) (\ls -> PRRspGetPorts ls)
        return ()
      _ -> 
        do
        infoM localLogger $ "dispatch: unknown request " ++ show msg
        handleRequest replyPort (return ()) (\_ -> PRRspUnknown)



-- ----------------------------------------------------------------------------
-- Typeclass instanciation (PortRegistry)
-- ----------------------------------------------------------------------------


-- The PortRegistry-typeclass instanciation for the PortRegistryData.
instance PortRegistry PortRegistryData where
  
  registerPort sn soid prd
    = do
      infoM localLogger $ "register: " ++ sn ++ " - at: " ++ show soid 
      modifyMVar (prd_SocketMap prd) $
        \sm -> return (Map.insert sn soid sm, ())

  unregisterPort sn prd
    = do
      infoM localLogger $ "unregister: " ++ sn
      modifyMVar (prd_SocketMap prd) $
        \sm -> return (Map.delete sn sm, ())
  
  lookupPort sn prd
    = do
      infoM localLogger $ "looking up: " ++ sn
      soid <- withMVar (prd_SocketMap prd) $
        \sm -> return (Map.lookup sn sm)
      infoM localLogger $ "result for: " ++ sn ++ " -> " ++ show soid
      return soid
  
  getPorts prd
    = do
      infoM localLogger $ "getting all ports"
      withMVar (prd_SocketMap prd) $
        \sm -> return (Map.toList sm)
      
