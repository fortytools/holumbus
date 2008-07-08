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

import Holumbus.Network.Port
import Holumbus.Distribution.Messages
import Holumbus.Distribution.Worker


-- ----------------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------------


data WorkerPort = WorkerPort WorkerRequestPort
  deriving (Show)


-- | Creates a new NodePort.
newWorkerPort :: WorkerRequestPort -> WorkerPort
newWorkerPort p = WorkerPort p



-- ----------------------------------------------------------------------------
-- Typeclass instanciation
-- ----------------------------------------------------------------------------


instance Worker WorkerPort where
  
  
  getWorkerRequestPort (WorkerPort p) = p

  startTask td w@(WorkerPort p)
    = do
      withStream $
        \s -> performPortAction p s (WReqStartTask td) $
          \rsp ->
          do
          case rsp of
            (WRspSuccess) -> return (Just w)
            _ -> return Nothing

  printDebug (WorkerPort p)
      = do
        putStrLn "WorkerPort:"
        putStrLn $ show p