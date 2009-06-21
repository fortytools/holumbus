-- ----------------------------------------------------------------------------
{- |
  Module     : Examples2.SimpleMR.SimpleDMapReduce
  Copyright  : Copyright (C) 2009 Sebastian Reese
  License    : MIT

  Maintainer : Sebastian Reese (str@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
-}
-- ----------------------------------------------------------------------------
module Holumbus.Distribution.SimpleDMapReduce
(
   SimpleMapFunction
 , SimpleReduceFunction
 , simpleClient
 , simpleWorker
)
where

import           Holumbus.Network.PortRegistry.PortRegistryPort
import           Holumbus.MapReduce.Types
import           Data.Binary
import           Control.Parallel.Strategies
import qualified Holumbus.Distribution.DMapReduce               as MR
import qualified Holumbus.FileSystem.FileSystem                 as FS
import qualified Holumbus.MapReduce.DaemonInterface             as DI
import qualified Holumbus.Data.KeyMap                           as KMap
import           Holumbus.Common.Logging
import           Holumbus.Common.Utils ( handleAll )
import           System.Log.Logger
import           System.Environment
import           System.Exit

{- ---------------------------------------------------------------------------------------------------------
   The simple types & fucntions
  --------------------------------------------------------------------------------------------------------- -}   

type SimpleMapFunction    k1 v1 k2 v2 =  k1 -> v1 -> (k2, v2)
type SimpleReduceFunction k2 v2 v3    =  k2 -> [v2] -> v3

{-
  The mapping function

  type MapFunction a k1 v1 k2 v2 = ActionEnvironment -> a -> k1 -> v1 -> IO [(k2, v2)]

-}
simpleMapping :: SimpleMapFunction k1 v1 k2 v2 -> MapFunction () k1 v1 k2 v2
simpleMapping f _ _ key1 value1 = return [(key2, value2 )]
  where
  (key2, value2) = f key1 value1

{-
 The reduce function

 type ReduceFunction a k2 v3 v4 = ActionEnvironment -> a -> k2 -> [v3] -> IO (Maybe v4)
-}
simpleReduce :: SimpleReduceFunction k2 v3 v4 -> ReduceFunction () k2 v3 v4
simpleReduce f _ _ key2 values3 = return ( Just f' )
  where 
  f' = f key2 values3

{-
actionConfig
-}
actionConfig :: (NFData k1, NFData k2, Ord k2, Binary k1, Binary k2, NFData v1, NFData v3, Binary v1, Binary v3, Binary v2, Binary v4) => SimpleMapFunction k1 v1 k2 v2 -> SimpleReduceFunction k2 v3 v4 -> ActionConfiguration () k1 v1 k2 v2 v3 v4
actionConfig m r = (defaultActionConfiguration "ID") {
           ac_Map     = Just . defaultMapConfiguration    $ simpleMapping m
         , ac_Reduce  = Just . defaultReduceConfiguration $ simpleReduce r
         }

{- ---------------------------------------------------------------------------------------------------------
   The simple client fucntions
  --------------------------------------------------------------------------------------------------------- -}   
  
  
{-
 The simple client
 
 doMapReduce :: ActionConfiguration a k1 v1 k2 v2 v3 v4
  -> a               -- ^ options
  -> [(k1,v1)]       -- ^ input (Tuples)
  -> [FileId]        -- ^ input (Files)
  -> Int             -- ^ number of splitters
  -> Int             -- ^ number of mappers
  -> Int             -- ^ number of reducers
  -> Int             -- ^ number of results
  -> TaskOutputType  -- ^ type of the result (file of raw)
  -> mr -> IO ([(k2,v4)],[FileId])
-}
simpleClient :: (NFData k1, NFData k2, Ord k2, Binary k1, Binary k2, NFData v1, NFData v3, Binary v1, Binary v3, Binary v2, Binary v4) => SimpleMapFunction k1 v1 k2 v2 -> SimpleReduceFunction k2 v3 v4 -> Int -> [(k1,v1)] -> IO [(k2,v4)]
simpleClient m r num ls = do
      p <- newPortRegistryFromXmlFile "/tmp/registry.xml"
      setPortRegistry p      
      mr <- initializeData
      (result, _) <- MR.doMapReduce (actionConfig m r) () ls [] num num num num TOTRawTuple mr
      deinitializeData mr
      return result

{-
The simple client's init function
-}
initializeData :: IO (MR.DMapReduce)
initializeData 
  = do    
    let config = MR.defaultMRClientConfig
    MR.mkMapReduceClient config

{-
The simple client's deinit function
-}
deinitializeData :: MR.DMapReduce -> IO ()
deinitializeData mr
  = do
    MR.closeMapReduce mr

{- ---------------------------------------------------------------------------------------------------------
   The simple worker fucntions
  --------------------------------------------------------------------------------------------------------- -}   
version :: String
version = "SimpleWorker v 0.1"

prompt :: String
prompt = "# "++version++" > "

localLogger :: String
localLogger = "Holumbus.Distribution..SimpleDMapReduce.simpleWorker"

pUsage :: IO ()
pUsage = do
  putStrLn "Usage: SimpleWorker ConsolePort Logfile"

params :: IO [String]
params = do
  args <- getArgs
  if length args /= 2 then do
    errorM localLogger "Wrong argument count"
    pUsage
    exitFailure
    else
      return args

{-
 The simpleWorker
-}
simpleWorker :: (NFData k1, NFData k2, Ord k2, Binary k1, Binary k2, NFData v1, NFData v3, Binary v1, Binary v3, Binary v2, Binary v4, Show v4, Show v3) => SimpleMapFunction k1 v1 k2 v2  -> SimpleReduceFunction k2 v3 v4 -> IO ()
simpleWorker m r = do
  handleAll (\e -> errorM localLogger $ "EXCEPTION: " ++ show e) $
    do 
    (s_cport:logfile:[]) <- params
    initializeFileLogging logfile [(localLogger, INFO),("Holumbus.Network.DoWithServer",INFO)]
    p <- newPortRegistryFromXmlFile "/tmp/registry.xml"
    setPortRegistry p
    (mr,fs) <- initSimpleWorker m r
    DI.runDaemon mr version (read s_cport) prompt 
    deinitSimpleWorker (mr,fs)

{-
 The simpleWorker's init functin
-}
initSimpleWorker :: (NFData k1, NFData k2, Ord k2, Binary k1, Binary k2, NFData v1, NFData v3, Binary v1, Binary v3, Binary v2, Binary v4) => SimpleMapFunction k1 v1 k2 v2  -> SimpleReduceFunction k2 v3 v4 -> IO (MR.DMapReduce, FS.FileSystem)
initSimpleWorker m r
  = do
    fs <- FS.mkFileSystemNode FS.defaultFSNodeConfig
    mr <- MR.mkMapReduceWorker fs actionMap MR.defaultMRWorkerConfig
    return (mr,fs)
    where
      actionMap :: ActionMap
      actionMap = KMap.insert (readActionConfiguration (actionConfig m r)) KMap.empty

{-
 The simpleWorker's deinit functin
-}
deinitSimpleWorker :: (MR.DMapReduce, FS.FileSystem) -> IO ()
deinitSimpleWorker (mr,fs)
  = do
    MR.closeMapReduce mr
    FS.closeFileSystem fs
    