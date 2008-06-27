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
)
where

--import Holumbus.Network.Site
import Holumbus.Network.Port

import Holumbus.FileSystem.Messages
import Holumbus.FileSystem.Controller

--import qualified Holumbus.FileSystem.Storage as S


-- ----------------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------------


data ControllerPort = ControllerPort ControllerRequestPort
  deriving (Show)


-- | Creates a new ControllerPort
newControllerPort :: ControllerRequestPort -> ControllerPort
newControllerPort p = ControllerPort p


-- ----------------------------------------------------------------------------
-- Typeclass instanciation
-- ----------------------------------------------------------------------------


instance Controller ControllerPort where
  
  getFileIds _ _ = undefined
  
  getControllerRequestPort (ControllerPort p) = p
  
  
  registerNode sid po c@(ControllerPort p) 
    = do
      withStream $
        \s -> performPortAction p s (CReqRegister sid po) $
          \rsp ->
          do
          case rsp of
            (CRspRegister n) -> return (Just (n,c))
            _ -> return Nothing
  

  unregisterNode nodeId c@(ControllerPort p)
    = do
      withStream $
        \s -> performPortAction p s (CReqUnregister nodeId) $
          \rsp ->
          do
          case rsp of
            (CRspUnregister) -> return (Just c)
            _ -> return Nothing
  
  
  getFileSites f (ControllerPort p)  
    = do
      withStream $
        \s -> performPortAction p s (CReqGetFileSites f) $
          \rsp ->
          do
          case rsp of
            (CRspGetFileSites set) -> return (Just set)
            _ -> return Nothing

  
  containsFile f (ControllerPort p)
    = do
      withStream $
        \s -> performPortAction p s (CReqContains f) $
          \rsp ->
          do
          case rsp of
            (CRspContains b) -> return (Just b)
            _ -> return Nothing
  
  
  getNearestNodePortWithFile f sid (ControllerPort p)
    = do
      withStream $
        \s -> performPortAction p s (CReqGetNearestNodePortWithFile f sid) $
          \rsp ->
          do
          case rsp of
            (CRspGetNearestNodePortWithFile po) -> return (Just po)
            _ -> return Nothing

  
  getNearestNodePortForFile f l sid (ControllerPort p)
    = do
      withStream $
        \s -> performPortAction p s (CReqGetNearestNodePortForFile f l sid) $
          \rsp ->
          do
          case rsp of
            (CRspGetNearestNodePortForFile po) -> return (Just po)
            _ -> return Nothing
            
            
  createFile f nid (ControllerPort p)
    = do
      withStream $
        \s -> performPortAction p s (CReqCreate f nid) $
          \rsp ->
          do
          case rsp of
            (CRspSuccess) -> return (Just $ ControllerPort p)
            _ -> return Nothing


  appendFile f nid (ControllerPort p)
    = do
      withStream $
        \s -> performPortAction p s (CReqAppend f nid) $
          \rsp ->
          do
          case rsp of
            (CRspSuccess) -> return (Just $ ControllerPort p)
            _ -> return Nothing


  deleteFile f nid (ControllerPort p)
    = do
      withStream $
        \s -> performPortAction p s (CReqDelete f nid) $
          \rsp ->
          do
          case rsp of
            (CRspSuccess) -> return (Just $ ControllerPort p)
            _ -> return Nothing

            
  printDebug (ControllerPort p)
    = do
      putStrLn "ControllerPort:"
      putStrLn $ show p