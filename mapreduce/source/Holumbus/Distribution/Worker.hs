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
  WorkerClass(..)
)
where

import qualified Holumbus.MapReduce.Types as T

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


class WorkerClass w where

  closeWorker :: w -> IO ()

  startTask :: T.TaskData -> w -> IO w

  stopTask :: T.TaskId -> w -> IO w

  stopAllTasks :: w -> IO w
  
  getActionNames :: w -> IO [T.ActionName]