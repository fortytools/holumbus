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

import Holumbus.Network.Port

import Holumbus.FileSystem.Messages
import Holumbus.FileSystem.Node


-- ----------------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------------


data NodePort = NodePort NodeRequestPort
  deriving (Show)


-- | Creates a new NodePort.
newNodePort :: NodeRequestPort -> NodePort
newNodePort p = NodePort p




-- ----------------------------------------------------------------------------
-- Typeclass instanciation
-- ----------------------------------------------------------------------------


instance Node NodePort where
  
  
  getNodeRequestPort (NodePort p) = p

  
  createFile i c (NodePort p)
    = do
      withStream $
        \s -> performPortAction p s (NReqCreate i c) $
          \rsp ->
          do
          case rsp of
            (NRspSuccess) -> return (Just $ NodePort p)
            _ -> return Nothing


  appendFile i c (NodePort p)
    = do
      withStream $
        \s -> performPortAction p s (NReqAppend i c) $
          \rsp ->
          do
          case rsp of
            (NRspSuccess) -> return (Just $ NodePort p)
            _ -> return Nothing


  deleteFile i b (NodePort p)
    = do
      withStream $
        \s -> performPortAction p s (NReqDelete i b) $
          \rsp ->
          do
          case rsp of
            (NRspSuccess) -> return (Just $ NodePort p)
            _ -> return Nothing         


  containsFile i (NodePort p)
    = do
      withStream $
        \s -> performPortAction p s (NReqContains i) $
          \rsp ->
          do
          case rsp of
            (NRspContains b) -> return (Just b)
            _ -> return Nothing


  getFileContent i (NodePort p)
    = do
      withStream $
        \s -> performPortAction p s (NReqGetFileContent i) $
          \rsp ->
          do
          case rsp of
            (NRspGetFileContent c) -> return (Just c)
            _ -> return Nothing

    
  getFileData i (NodePort p)
    = do
      withStream $
        \s -> performPortAction p s (NReqGetFileData i) $
          \rsp ->
          do
          case rsp of
            (NRspGetFileData d) -> return (Just d)
            _ -> return Nothing


  getFileIds (NodePort p)
    = do
      withStream $
        \s -> performPortAction p s (NReqGetFileIds) $
          \rsp ->
          do
          case rsp of
            (NRspGetFileIds ls) -> return (Just ls)
            _ -> return Nothing


  printDebug (NodePort p)
    = do
      putStrLn "NodePort:"
      putStrLn $ show p