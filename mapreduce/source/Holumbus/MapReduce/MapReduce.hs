-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.MapReduce.MapReduce
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.MapReduce.MapReduce
(
  MapReduce(..)
)
where

import Holumbus.MapReduce.Types
import Holumbus.Network.Site


class MapReduce mr where
  
  getMySiteId :: mr -> IO (SiteId)
  
  addJob :: JobInfo -> mr -> IO ()

  doSingleStep :: mr -> IO ()
  
  printDebug :: mr -> IO ()