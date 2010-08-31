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
module Holumbus.MapReduce.Examples.MandelbrotSet.SimpleDMapReduceIO	
(
   MapFunction
 , ReduceFunction
 , client
 , worker
 , partition'
 , SplitF
 , MapF
 , ReduceF
 , A
 , K1
 , K2
 , V1
 , V2
 , V3
 , V4 
 , Priority(..)
 , putTimeStamp
)
where

import           Holumbus.Network.PortRegistry.PortRegistryPort
import           Holumbus.MapReduce.Types
import           Holumbus.Common.FileHandling
import           Data.Binary
import qualified Data.ByteString.Lazy as B
--import           Holumbus.Common.MRBinary
import           Data.Maybe
import           Control.DeepSeq
import qualified Holumbus.Distribution.DMapReduce               as MR
import qualified Holumbus.FileSystem.FileSystem                 as FS
import qualified Holumbus.MapReduce.DaemonInterface             as DI
import qualified Holumbus.Data.KeyMap                           as KMap
import           Holumbus.Common.Logging
import           Holumbus.Common.Utils ( handleAll )
import           System.Log.Logger
import           System.Environment
import           System.Exit
import Data.Time.Clock.POSIX
import Holumbus.MapReduce.Examples.MandelbrotSet.ImageTypes hiding (Image)

-- the x,y coordinates
type XCoord = Int
type YCoord = Int

-- the block id 
type BlockID   = Int

-- a row of the image
type Line a = (YCoord, [a])

-- the image itself
type Image  a = [Line a]
 
-- the options, which are Width, Height, Z-factor, Repetitions
type Options = (Int, Int, Double, Int)

-- the actual image send over to the splitters and workers
type BlockImage a = (BlockID, Image a)
--type SplitImage = (SplitID, Image Lightness)
--type MapImage = (MapID, Image XCoord)
 
type A  = Options
type K1 = BlockID
type K2 = BlockID
type V1 = Image XCoord
type V2 = Image Lightness
type V3 = V2
type V4 = (Image Lightness)

type SplitF  = SplitFunction A K1 V1
type MapF    = MapFunction A K1 V1 K2 V2
type ReduceF = ReduceFunction A K2 V3 V4


mapConfiguration :: MapF -> MapConfiguration A K1 V1 K2 V2
mapConfiguration fct = MapConfiguration fct hashedPartition defaultInputReader defaultOutputWriter


reduceConfiguration :: ReduceF -> ReduceConfiguration A K2 V3 V4
reduceConfiguration fct = ReduceConfiguration defaultMerge fct hashedPartition defaultInputReader defaultOutputWriter

{-
actionConfig
-}
actionConfig :: MapF -> ReduceF -> ActionConfiguration A K1 V1 K2 V2 V3 V4
actionConfig m r = (defaultActionConfiguration "MANDELBROT_3") {
           ac_Map    = Just . mapConfiguration    $ m
         , ac_Reduce = Nothing -- Just . reduceConfiguration $ r
         , ac_Split  = Nothing -- Just splitConfiguration
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
client :: MapF -> ReduceF -> Options -> (Int,Int) -> [[(K1,V1)]] -> IO [(K2,V4)]
client m r a (mappers,reducers) lss = do
      -- create port registry
      p <- newPortRegistryFromXmlFile "/tmp/registry.xml"
      setPortRegistry p      
      
      -- create mapreduce data
      mr <- initializeData
      
      -- make filesystem
      fs <- FS.mkFileSystemClient FS.defaultFSClientConfig
      -- create the filenames and store the data to the map reduce filesystem
--      let filenames = map (\i -> "initial_input_"++show i) [1..(length lss)]
--      mapM_ (\(filename,ls) -> FS.createFile filename (listToByteString ls) fs) $ zip filenames lss

      let (files,filenames) = prepareFiles lss 0
      FS.createFiles files fs

      
      -- do the map reduce job
      t1 <- getPOSIXTime
      putStrLn ("Begin MR: " ++ show t1)
      (_,fids) <- MR.doMapReduce (actionConfig m r) a [] filenames 1 mappers reducers 1 TOTFile mr
      t2 <- getPOSIXTime
      putStrLn ("END MR: " ++ show t2)
      
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
worker :: MapF  -> ReduceF -> [(String,Priority)] -> IO ()
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
initWorker :: MapF -> ReduceF -> IO (MR.DMapReduce, FS.FileSystem)
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
