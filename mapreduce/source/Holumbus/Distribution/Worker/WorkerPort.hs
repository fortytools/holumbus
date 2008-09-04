-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Distribution.Worker.WorkerPort
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.Distribution.Worker.WorkerPort
(
-- * Datatypes
  WorkerPort
  
-- * Creation and Destruction
, newWorkerPort
)
where

import Holumbus.Common.Debug
import Holumbus.Network.Communication
import Holumbus.Distribution.Messages
import Holumbus.Distribution.Worker


-- ----------------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------------


data WorkerPort = WorkerPort ClientPort
  deriving (Show)


-- | Creates a new NodePort.
newWorkerPort :: ClientPort -> WorkerPort
newWorkerPort p = WorkerPort p



-- ----------------------------------------------------------------------------
-- Typeclass instanciation
-- ----------------------------------------------------------------------------


instance WorkerClass WorkerPort where
  
  closeWorker _ = return ()


  startTask td w@(WorkerPort p)
    = do
      sendRequestToClient p time30 (WReqStartTask td) $
          \rsp ->
          do
          case rsp of
            (WRspSuccess) -> return (Just w)
            _ -> return Nothing


  stopTask tid w@(WorkerPort p)
    = do
      sendRequestToClient p time30 (WReqStopTask tid) $
          \rsp ->
          do
          case rsp of
            (WRspSuccess) -> return (Just w)
            _ -> return Nothing


  stopAllTasks w@(WorkerPort p)
    = do
      sendRequestToClient p time30 (WReqStopAllTasks) $
          \rsp ->
          do
          case rsp of
            (WRspSuccess) -> return (Just w)
            _ -> return Nothing


  getActionNames (WorkerPort p)
    = do
      sendRequestToClient p time30 (WReqGetActionNames) $
          \rsp ->
          do
          case rsp of
            (WRspGetActionNames as) -> return (Just as)
            _ -> return Nothing



instance Debug WorkerPort where
  printDebug (WorkerPort p)
      = do
        putStrLn "WorkerPort:"
        putStrLn $ show p