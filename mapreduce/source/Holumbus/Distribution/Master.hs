-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Distribution.Master
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.Distribution.Master
(
  Master(..)
)
where


import qualified Holumbus.Distribution.Messages as M
import Holumbus.Network.Site



-- ----------------------------------------------------------------------------
-- Typeclass
-- ----------------------------------------------------------------------------


class Master m where

  getMasterRequestPort :: m -> M.MasterRequestPort
  
  registerWorker :: SiteId -> M.WorkerRequestPort -> m -> IO (M.WorkerId, m)
  
  unregisterWorker :: M.WorkerId -> m -> IO m

  printDebug :: m -> IO ()