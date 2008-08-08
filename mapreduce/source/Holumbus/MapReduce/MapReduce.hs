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
  MapReduceType(..)
, MapReduce(..)
)
where

import           Holumbus.Common.Debug
import           Holumbus.MapReduce.Types
import           Holumbus.Network.Site


data MapReduceType = MRTMaster | MRTWorker | MRTClient | MRTStandalone
  deriving (Show, Eq, Ord, Enum)
  

class (Debug mr) => MapReduce mr where
  
  -- | prints the siteId of the MapReduce instance 
  getMySiteId :: mr -> IO SiteId
  
  -- | get the Type of the MapReduce instance
  getMapReduceType :: mr -> IO MapReduceType
  
  -- | get the Controlling-Type (normal or singlestep) of the MapReduce instance
  startControlling :: mr -> IO ()

  -- | get the Controlling-Type (normal or singlestep) of the MapReduce instance
  stopControlling :: mr -> IO ()

  -- | test, if Controller is running
  isControlling :: mr -> IO Bool
  
  -- | performs a single step of the controller (if mode is singlestep)
  doSingleStep :: mr -> IO ()
  
  -- | starts a MapReduce-Job (blocking while finished)
  doMapReduce :: JobInfo -> mr -> IO JobResult