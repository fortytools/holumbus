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
import qualified Holumbus.MapReduce.JobController as J

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


class Worker w where

  getWorkerRequestPort :: w -> M.WorkerRequestPort

  startTask :: J.TaskData -> w -> IO w

  printDebug :: w -> IO ()
  