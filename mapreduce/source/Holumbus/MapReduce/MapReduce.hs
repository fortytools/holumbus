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
import           Holumbus.FileSystem.FileSystem
import           Holumbus.MapReduce.Types
import           Holumbus.Network.Site


data MapReduceType = MRTMaster | MRTWorker | MRTClient | MRTStandalone
  deriving (Show, Eq, Ord, Enum)
  

class (Debug mr) => MapReduce mr where
  
  closeMapReduce :: mr -> IO ()
  
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
  doMapReduceJob :: JobInfo -> mr -> IO JobResult
  
  doMapReduce
    :: ActionConfiguration a k1 v1 k2 v2 v3 v4
    -> a               -- ^ options
    -> [(k1,v1)]       -- ^ input (Tuples)
    -> [FileId]        -- ^ input (Files)
    -> Int             -- ^ number of splitters
    -> Int             -- ^ number of mappers
    -> Int             -- ^ number of reducers
    -> Int             -- ^ number of results
    -> TaskOutputType  -- ^ type of the result (file of raw)
    -> mr -> IO ([(k2,v4)],[FileId])
  doMapReduce c o ls1 ls2 sc mc rc nor rt mr
    = do
      let ji = createJobInfoFromConfiguration c o ls1 ls2 sc mc rc nor rt
      jr <- doMapReduceJob ji mr
      let res = createListsFromJobResult c jr
      return res
  
