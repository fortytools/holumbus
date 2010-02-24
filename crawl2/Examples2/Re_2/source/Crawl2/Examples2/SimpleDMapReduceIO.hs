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
module Crawl2.Examples2.SimpleDMapReduceIO
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
import           Holumbus.MapReduce.Types hiding (defaultSplit)
import           Holumbus.Common.FileHandling
import           Data.Maybe
import       qualified    Data.Map as M
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
import Crawl2.Examples2.Common

splitConfiguration :: SplitConfiguration Options K1 V1
splitConfiguration
  = SplitConfiguration
      defaultSplit
      defaultInputReader
      defaultOutputWriter

mapConfiguration :: MapFunction Options K1 V1 K2 V2 -> MapConfiguration Options K1 V1 K2 V2
mapConfiguration fct
  = MapConfiguration
      fct
      hashedPartition
      defaultInputReader
      defaultOutputWriter

reduceConfiguration :: ReduceFunction Options K2 V3 V4 -> ReduceConfiguration Options K2 V3 V4
reduceConfiguration fct
  = ReduceConfiguration
      defaultMerge
      fct
      hashedPartition
      defaultInputReader
      defaultOutputWriter

defaultSplit :: SplitFunction Options K1 V1
defaultSplit _ _ n l = do
  let res = f l
  infoM localLogger $ "defaultSplit: " ++ (show . length) res
  return res
    where
    f :: [(K1,V1)] -> [(Int,[(K1,V1)])]
    f l = concatMap (\(k1,(state, urimap)) ->
      let urilist = M.toList urimap;
          partitionedUris = partitionUris urilist;
          urimapswithstatesnkeys = map (\urilist' -> [(k1,(state, M.fromList urilist'))]) partitionedUris -- [[(k1,v1)]]
          in zip ns urimapswithstatesnkeys) l
    partitionUris l = part n (length l) l
    ns = [(x `mod` n) + 1 | x <- [0..]]

part :: Int -> Int -> [a] -> [[a]]
part parts len images = part' parts (len `div` parts) 1 images
  where
  part' :: Int -> Int -> Int -> [a] -> [[a]]
  part' parts2 size i l
    | i == parts = [l]
    | otherwise  = (fst : part' parts2 size i' rst)
    where
    (fst,rst) = splitAt size l
    i' = i + 1
    
{-
actionConfig
-}
actionConfig :: MapFunction Options K1 V1 K2 V2 -> ReduceFunction Options K2 V3 V4 -> ActionConfiguration Options K1 V1 K2 V2 V3 V4
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
 
 doMapReduce :: ActionConfiguration Options K1 V1 K2 V2 V3 V4
  -> Options               -- ^ options
  -> [(K1,V1)]       -- ^ input (Tuples)
  -> [FileId]        -- ^ input (Files)
  -> Int             -- ^ number of splitters
  -> Int             -- ^ number of mappers
  -> Int             -- ^ number of reducers
  -> Int             -- ^ number of results
  -> TaskOutputType  -- ^ type of the result (file of raw)
  -> mr -> IO ([(K2,V4)],[FileId])
-}
client :: MapFunction Options K1 V1 K2 V2 -> ReduceFunction Options K2 V3 V4 -> Options -> (Int,Int,Int) -> [[(K1,V1)]] -> IO [(K2,V4)]
client m r a (splitters,mappers,reducers) lss = do
      -- create port registry
      p <- newPortRegistryFromXmlFile "/tmp/registry.xml"
      setPortRegistry p      
      
      -- create mapreduce data
      mr <- initializeData
      
      -- make filesystem
      fs <- FS.mkFileSystemNode FS.defaultFSNodeConfig
      -- create the filenames and store the data to the map reduce filesystem
      let filenames = map (\i -> "initial_input_"++show i) [1..(length lss)]
      mapM_ (\(filename,ls) -> FS.createFile filename (listToByteString ls) fs) $ zip filenames lss
      
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
      
merge :: [FS.FileId] -> FS.FileSystem -> IO [(K2,V4)]
merge fids fs = do
   mayberesult <- mapM ( flip FS.getFileContent fs) fids
   let result = concat . map parseByteStringToList $ catMaybes mayberesult
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
worker :: MapFunction Options K1 V1 K2 V2  -> ReduceFunction Options K2 V3 V4 -> [(String,Priority)] -> IO ()
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
initWorker :: MapFunction Options K1 V1 K2 V2  -> ReduceFunction Options K2 V3 V4 -> IO (MR.DMapReduce, FS.FileSystem)
initWorker m r
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
partition'    us   (xs:[]) = [us]
partition' (u:us) (xs:xss) = partition' us (xss ++ [xs'])
  where xs' = (u:xs)

putTimeStamp :: String -> IO ()
putTimeStamp s = do
  t1 <- getPOSIXTime
  putStrLn (s++" : "++ show t1)
