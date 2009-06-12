-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.FileSystem.Node.NodePort
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}

-- ----------------------------------------------------------------------------

module Holumbus.FileSystem.Node.NodePort
(
-- * Datatypes
  NodePort
  
-- * Creation and Destruction
, newNodePort
)
where

import Holumbus.Common.Debug
import Holumbus.Network.Communication
import Holumbus.FileSystem.Messages
import Holumbus.FileSystem.Node


-- ----------------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------------


data NodePort = NodePort ClientPort
  deriving (Show)


-- | Creates a new NodePort.
newNodePort :: ClientPort -> NodePort
newNodePort p = NodePort p




-- ----------------------------------------------------------------------------
-- Typeclass instanciation
-- ----------------------------------------------------------------------------


instance NodeClass NodePort where
  
  closeNode _ = return ()
  
  
  createFile i c (NodePort p)
    = do
      sendRequestToClient p time30 (NReqCreate i c) $
        \rsp ->
        do
        case rsp of
            (NRspSuccess) -> return (Just ())
            _ -> return Nothing


  appendFile i c (NodePort p)
    = do
      sendRequestToClient p time30 (NReqAppend i c) $
        \rsp ->
        do
        case rsp of
          (NRspSuccess) -> return (Just ())
          _ -> return Nothing


  deleteFile i b (NodePort p) 
    = do
      sendRequestToClient p time30 (NReqDelete i b) $
        \rsp ->
        do
        case rsp of
          (NRspSuccess) -> return (Just ())
          _ -> return Nothing


  copyFile i cp (NodePort p)
    = do
      sendRequestToClient p time30 (NReqCopy i cp) $
        \rsp ->
        do
        case rsp of
          (NRspSuccess) -> return (Just ())
          _ -> return Nothing


  containsFile i (NodePort p)
    = do
      sendRequestToClient p time30 (NReqContains i) $
        \rsp ->
        do
        case rsp of
          (NRspContains b) -> return (Just b)
          _ -> return Nothing


  getFileContent i (NodePort p)
    = do
      sendRequestToClient p time30 (NReqGetFileContent i) $
        \rsp ->
        do
        case rsp of
          (NRspGetFileContent c) -> return (Just c)
          _ -> return Nothing

    
  getFileData i (NodePort p)
    = do
      sendRequestToClient p time30 (NReqGetFileData i) $
        \rsp ->
        do
        case rsp of
          (NRspGetFileData d) -> return (Just d)
          _ -> return Nothing


  getFileIds (NodePort p)
    = do
      sendRequestToClient p time30 (NReqGetFileIds) $
        \rsp ->
        do
        case rsp of
          (NRspGetFileIds ls) -> return (Just ls)
          _ -> return Nothing


instance Debug NodePort where
  printDebug (NodePort p)
    = do
      putStrLn "NodePort:"
      putStrLn $ show p
  getDebug (NodePort p)
    = return ("NodePort:\n"++show p++"\n")