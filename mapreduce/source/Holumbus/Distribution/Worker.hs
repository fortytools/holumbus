-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Distribution.Worker
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.Distribution.Worker
(
  Worker(..)
)
where

import qualified Holumbus.Distribution.Messages as M
import qualified Holumbus.MapReduce.Types as T

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


class Worker w where

  closeWorker :: w -> IO ()

  getWorkerRequestPort :: w -> M.WorkerRequestPort

  startTask :: T.TaskData -> w -> IO w

  stopTask :: T.TaskId -> w -> IO w

  stopAllTasks :: w -> IO w

  printDebug :: w -> IO ()
  