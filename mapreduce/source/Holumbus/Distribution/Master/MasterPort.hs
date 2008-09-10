-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Distribution.Master.MasterPort
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.Distribution.Master.MasterPort
(
-- * Datatypes
  MasterPort
  
-- * Creation and Destruction
, newMasterPort
, newMasterPortFromServerPort
)
where

import Holumbus.Network.Communication
import Holumbus.Network.Site

import Holumbus.Common.Debug
import Holumbus.MapReduce.MapReduce
import Holumbus.Distribution.Messages
import Holumbus.Distribution.Master


-- ----------------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------------


data MasterPort = MasterPort ServerPort
  deriving (Show)



-- ----------------------------------------------------------------------------
-- Creation and Destruction
-- ----------------------------------------------------------------------------


-- | Creates a new ControllerPort
newMasterPort :: StreamName -> Maybe SocketId -> IO MasterPort
newMasterPort sn soid
  = do
    sp <- newServerPort sn soid
    return (MasterPort sp)

newMasterPortFromServerPort :: ServerPort -> MasterPort
newMasterPortFromServerPort sp = MasterPort sp


-- ----------------------------------------------------------------------------
-- Typeclass instanciation
-- ----------------------------------------------------------------------------

instance Debug MasterPort where

  printDebug (MasterPort p)
    = do
      putStrLn "MasterPort:"
      putStrLn $ show p
      
      
     

instance MapReduce MasterPort where


  closeMapReduce md
    = closeMaster md


  getMySiteId _
    = getSiteId

  
  getMapReduceType _
    = return MRTMaster

  
  startControlling (MasterPort p)
    = do
      sendRequestToServer p time30 (MReqStartControlling) $
         \rsp ->
         do
         case rsp of
           (MRspSuccess) -> return (Just ())
           _ -> return Nothing

  
  stopControlling (MasterPort p)
    = do
      sendRequestToServer p time30 (MReqStopControlling) $
         \rsp ->
         do
         case rsp of
           (MRspSuccess) -> return (Just ())
           _ -> return Nothing


  isControlling (MasterPort p)
    = do
      sendRequestToServer p time30 (MReqIsControlling) $
         \rsp ->
         do
         case rsp of
           (MRspIsControlling b) -> return (Just b)
           _ -> return Nothing
  

  
  doSingleStep (MasterPort p)
    = do
      sendRequestToServer p time30 (MReqSingleStep) $
          \rsp ->
          do
          case rsp of
            (MRspSuccess) -> return (Just ())
            _ -> return Nothing

  
  doMapReduce ji (MasterPort p)
    = do
      -- bad hack... we should build an extra client for the MapReduce-System
      -- which pings for the server and so on
      sendRequestToServer p timeIndefinitely (MReqPerformJob ji) $
          \rsp ->
          do
          case rsp of
            (MRspResult r) -> return (Just r)
            _ -> return Nothing




instance MasterClass MasterPort where


  closeMaster _ = return ()


  receiveTaskCompleted td c@(MasterPort p)
    = do
      sendRequestToServer p time30 (MReqTaskCompleted td) $
          \rsp ->
          do
          case rsp of
            (MRspSuccess) -> return (Just c)
            _ -> return Nothing
    

  receiveTaskError td c@(MasterPort p)
    = do
      sendRequestToServer p time30 (MReqTaskError td) $
          \rsp ->
          do
          case rsp of
            (MRspSuccess) -> return (Just c)
            _ -> return Nothing