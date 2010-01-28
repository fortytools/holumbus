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
module Holumbus.MapReduce.Examples.SimpleDMapReduceIO
(
   MapFunction
 , ReduceFunction
 , client
 , worker
 , partition'
 , Priority(..)
 , putTimeStamp
)
where

import           Holumbus.Network.PortRegistry.PortRegistryPort
import           Holumbus.MapReduce.Types
import           Holumbus.Common.FileHandling
import           Data.Binary
--import           Holumbus.Common.MRBinary
import           Data.Maybe
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
import           Data.Time.Clock.POSIX
import           System.Console.Readline
import qualified Data.ByteString.Lazy as B

splitConfiguration
  :: (Hash k1, NFData v1, NFData k1, Binary a, Binary k1, Binary v1)
  => SplitConfiguration a k1 v1
splitConfiguration
  = SplitConfiguration
      defaultSplit
      defaultInputReader
      defaultOutputWriter

mapConfiguration
  :: (Hash k2, NFData v1, NFData k1, NFData v2, NFData k2, Ord k2, Binary a, Binary k1, Binary v1, Binary k2, Binary v2)
  => MapFunction a k1 v1 k2 v2
  -> MapConfiguration a k1 v1 k2 v2
mapConfiguration fct
  = MapConfiguration
      fct
      hashedPartition
      defaultInputReader
      defaultOutputWriter

reduceConfiguration
  :: (Hash k2, NFData v2, NFData k2, NFData v3, Ord k2, Binary a, Binary k2, Binary v2, Binary v3)
  => ReduceFunction a k2 v2 v3
  -> ReduceConfiguration a k2 v2 v3
reduceConfiguration fct
  = ReduceConfiguration
      defaultMerge
      fct
      hashedPartition
      defaultInputReader
      defaultOutputWriter


{-
actionConfig
-}
actionConfig :: (Hash k1, Hash k2, Binary a, NFData k1, NFData k2, Ord k2, Binary k1, Binary k2, NFData v1, NFData v4, NFData v2, NFData v3, Binary v1, Binary v3, Binary v2, Binary v4) => MapFunction a k1 v1 k2 v2 -> ReduceFunction a k2 v3 v4 -> ActionConfiguration a k1 v1 k2 v2 v3 v4
actionConfig m r = (defaultActionConfiguration "ID") {
           ac_Split   = Just splitConfiguration  
         , ac_Map     = Just . mapConfiguration    $ m
         , ac_Reduce  = Just . reduceConfiguration $ r
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
client :: ( Show k1, Show k2, Show v1, Show v2, Show v3, Show v4
          , Binary v1, Binary v3, Binary v2, Binary v4
          , Binary a, Binary k1, Binary k2
          , NFData k1, NFData k2, NFData v1, NFData v4, NFData v2, NFData v3
          , Ord k2, Hash k1, Hash k2) =>
          MapFunction a k1 v1 k2 v2 -> ReduceFunction a k2 v3 v4
          -> a -> (Int,Int,Int) -> [[(k1,v1)]] -> IO [(k2,v4)]
client m r a (splitters,mappers,reducers) lss = do
      -- create port registry
      p <- newPortRegistryFromXmlFile "/tmp/registry.xml"
      setPortRegistry p      
      
      -- make filesystem
      fs <- FS.mkFileSystemClient FS.defaultFSClientConfig
      
      -- create mapreduce data
      mr <- initializeData
      
      --_ <- readline "Press enter to continue .."
      -- create the filenames and store the data to the map reduce filesystem
--      let filenames = map (\i -> "initial_input_"++show i) [1..(length lss)]
--      FS.createFiles (zipWith (\fn c -> (fn,listToByteString c)) filenames lss) fs
      let (files,filenames) = prepareFiles lss 0
      FS.createFiles files fs
      -- mapM_ (\(filename,ls) -> FS.createFile filename (listToByteString ls) fs) $ zip filenames lss
      
      -- do the map reduce job
      putTimeStamp "SimpleDMR Begin MR"
      (_,fids) <- MR.doMapReduce (actionConfig m r) a [] filenames splitters mappers reducers 1 TOTFile mr
      putTimeStamp "SimpleDMR End MR"
      
      -- get the results from filesystem
      result <- merge fids fs
      
      -- close file- and mapreducesystem
      deinitializeData mr
      FS.closeFileSystem fs
      
      -- finally, return the result
      return result

prepareFiles :: Binary a => [[a]] -> Int -> ([(String,B.ByteString)],[String])
prepareFiles []     _ = ([],[])
prepareFiles (x:xs) i = ((fn,bin):files,fn:filenames)
  where
  fn = ("Initial_input_"++show i)
  bin = listToByteString x
  (files,filenames) = prepareFiles xs (i+1)
      
merge :: (Show k2, Show v4, Hash k2, Binary k2, Binary v4, NFData k2, NFData v4) => [FS.FileId] -> FS.FileSystem -> IO [(k2,v4)]
merge fids fs = do
   mayberesult <- FS.getMultiFileContent fids fs
   let result = concatMap (parseByteStringToList . snd)  $ mayberesult
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
localLogger = "Holumbus.Distribution.SimpleDMapReduceIO.worker"

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
worker :: (Show k1, Show k2, Show v1, Show v2, Hash k1, Hash k2, Binary a, NFData k1, NFData k2, Ord k2, Binary k1, Binary k2, NFData v1,NFData v4, NFData v2, NFData v3, Binary v1, Binary v3, Binary v2, Binary v4, Show v4, Show v3) => MapFunction a k1 v1 k2 v2  -> ReduceFunction a k2 v3 v4 -> [(String,Priority)] -> IO ()
worker m r loggers = do
  handleAll (\e -> errorM localLogger $ "EXCEPTION: " ++ show e) $
    do 
    (s_cport:logfile:[]) <- params
    --initializeFileLogging logfile [(localLogger, INFO),("Holumbus.Network.DoWithServer",INFO),("Holumbus.MapReduce.Types", INFO)]
    initializeFileLogging logfile ([(localLogger, INFO),("Holumbus.Network.DoWithServer",INFO)]++loggers)
    p <- newPortRegistryFromXmlFile "/tmp/registry.xml"
    setPortRegistry p
    (mr,fs) <- initWorker m r
    DI.runDaemon mr version (read s_cport) prompt 
    deinitWorker (mr,fs)

{-
 The simpleWorker's init functin
-}
initWorker :: (Hash k1, Show k1, Show k2, Show v1, Show v2, Show v3, Show v4, Hash k2, Binary a, NFData k1, NFData k2, Ord k2, Binary k1, Binary k2, NFData v1, NFData v4, NFData v2, NFData v3, Binary v1, Binary v3, Binary v2, Binary v4) => MapFunction a k1 v1 k2 v2  -> ReduceFunction a k2 v3 v4 -> IO (MR.DMapReduce, FS.FileSystem)
initWorker m r
  = do
    fs <- FS.mkFileSystemNode FS.defaultFSNodeConfig
    --fs <- FS.mkFileSystemClient FS.defaultFSClientConfig
    mr <- MR.mkMapReduceWorker fs actionMap MR.defaultMRWorkerConfig
    return (mr,fs)
    where
      actionMap :: ActionMap
      actionMap = KMap.insert (readActionConfiguration (actionConfig m r)) KMap.empty

{-
 The simpleWorker's deinit functin
-}
deinitWorker :: (MR.DMapReduce, FS.FileSystem) -> IO ()
deinitWorker (mr,fs)
  = do
    MR.closeMapReduce mr
    FS.closeFileSystem fs
    
{-

partition'

first list, list of 
 -}
partition' :: [a] -> [[a]] -> [[a]]
partition'     _        [] = []
partition'    []       xss = xss
partition'    us   (_xs:[]) = [us]
partition' (u:us) (xs:xss) = partition' us (xss ++ [xs'])
  where xs' = (u:xs)

putTimeStamp :: String -> IO ()
putTimeStamp s = do
  t1 <- getPOSIXTime
  putStrLn (s++" : "++ show t1)
