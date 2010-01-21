-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.FileSystem.Controller.ControllerClass
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}

-- ----------------------------------------------------------------------------

module Holumbus.FileSystem.Controller.ControllerPort
(
-- * Datatypes
  ControllerPort
  
-- * Creation and Destruction
, newControllerPort
, newControllerPortFromServerPort
)
where

import Holumbus.Common.Debug
import Holumbus.Network.Communication
import Holumbus.FileSystem.Messages
import Holumbus.FileSystem.Controller


-- ----------------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------------


data ControllerPort = ControllerPort ServerPort
  deriving (Show)


-- | Creates a new ControllerPort
newControllerPort :: StreamName -> Maybe SocketId -> IO ControllerPort
newControllerPort sn soid
  = do
    sp <- newServerPort sn soid
    return (ControllerPort sp)

newControllerPortFromServerPort :: ServerPort -> ControllerPort
newControllerPortFromServerPort sp = ControllerPort sp

-- ----------------------------------------------------------------------------
-- Typeclass instanciation
-- ----------------------------------------------------------------------------


instance ControllerClass ControllerPort where
  
  closeController _ = return ()
  
    
  -- getServerPort (ControllerPort p) = p
  
  
  getFileSites f (ControllerPort p)  
    = do
      sendRequestToServer p time30 (CReqGetFileSites f) $
        \rsp ->
        do
        case rsp of
          (CRspGetFileSites set) -> return (Just set)
          _ -> return Nothing

  
  containsFile f (ControllerPort p)
    = do
      sendRequestToServer p time30 (CReqContains f) $
        \rsp ->
        do
        case rsp of
          (CRspContains b) -> return (Just b)
          _ -> return Nothing
  
  
  getNearestNodePortWithFile f sid (ControllerPort p)
    = do
      sendRequestToServer p time30 (CReqGetNearestNodePortWithFile f sid) $
        \rsp ->
        do
        case rsp of
          (CRspGetNearestNodePortWithFile po) -> return (Just po)
          _ -> return Nothing

  
  getNearestNodePortForFile f l sid (ControllerPort p)
    = do
      sendRequestToServer p time30 (CReqGetNearestNodePortForFile f l sid) $
        \rsp ->
        do
        case rsp of
          (CRspGetNearestNodePortForFile po) -> return (Just po)
          _ -> return Nothing
            
  getNearestNodePortForFiles l sid (ControllerPort p)
    = do
      sendRequestToServer p time30 (CReqGetNearestNodePortForFiles l sid) $
        \rsp ->
        do
        case rsp of
          (CRspGetNearestNodePortForFiles portlist) -> return (Just portlist)
          _ -> return Nothing
            
  createFile f nid (ControllerPort p)
    = do
      sendRequestToServer p time30 (CReqCreate f nid) $
        \rsp ->
        do
        case rsp of
          (CRspSuccess) -> return (Just $ ())
          _ -> return Nothing

  createFiles l (ControllerPort p)
    = do
      sendRequestToServer p time30 (CReqCreateS l) $
        \rsp ->
        do
        case rsp of
          (CRspSuccess) -> return (Just $ ())
          _ -> return Nothing


  appendFile f nid (ControllerPort p)
    = do
      sendRequestToServer p time30 (CReqAppend f nid) $
        \rsp ->
        do
        case rsp of
          (CRspSuccess) -> return (Just $ ())
          _ -> return Nothing


  deleteFile f nid (ControllerPort p)
    = do
      sendRequestToServer p time30 (CReqDelete f nid) $
        \rsp ->
        do
        case rsp of
          (CRspSuccess) -> return (Just $ ())
          _ -> return Nothing

            
            
instance Debug ControllerPort where            
  printDebug (ControllerPort p)
    = do
      putStrLn "ControllerPort:"
      putStrLn $ show p
  getDebug (ControllerPort p)
    = return ("ControllerPort:\n"++show p++"\n")
