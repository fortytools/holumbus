-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.MapReduce.Examples.Count.DCount 
  Copyright  : Copyright (C) 2009 Sebastian Reese
  License    : MIT

  Maintainer : Sebastian Reese (str@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
-}
-- ----------------------------------------------------------------------------
module Holumbus.MapReduce.Examples.Count.DCount
(
   MapFunction
 , ReduceFunction
 , worker
 , partition'
 , Priority(..)
 , countMap
 , countReduce
 , client
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


splitConfiguration
  :: (Hash k1, NFData v1, NFData k1, Binary a, Binary k1, Binary v1)
  => SplitConfiguration a k1 v1
splitConfiguration
  = SplitConfiguration
      hashedPartition
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
client :: IO ()
client = do
  -- get command line arguments
  (filename : _splitmapreduresult : wordsToCount : []) <- getArgs

  -- convert number of mappers, reducers...
  --let (splitters,mappers,reducers) = read splitmapreduresult;

  -- read the input textfile
  file <- readFile filename
  --let  splitted = zip ([0..]::[Integer]) (partition 100000 . words $ file)

  -- create port registry
  p <- newPortRegistryFromXmlFile "/tmp/registry.xml"
  setPortRegistry p      
      
  -- create mapreduce data
  mr <- initializeData
      
  -- make filesystem
  fs <- FS.mkFileSystemNode FS.defaultFSNodeConfig
  
  -- create the filenames and store the data to the map reduce filesystem
  mapM_ (\ls@(i,_) -> FS.createFile ("initial_input"++show i) (listToByteString [ls]) fs) $ [(0::Integer,file)]--splitted
  let filenames = map (\(i,_) -> "initial_input"++show i) [(0,file)] --splitted

  -- do the map reduce job
  --(_,fids) <- MR.doMapReduce (actionConfig countMap countReduce) (words wordsToCount) [] filenames splitters mappers reducers 1 TOTFile mr

  -- get the results from filesystem
  --result <- merge fids fs

  -- close file- and mapreducesystem
  deinitializeData mr
  FS.closeFileSystem fs

  -- debug
  --putStrLn . show $ result

  -- process the result and show it
  --let result' = sum . map snd $ result
  --putStrLn ("Occurence of word(s) \""++wordsToCount++"\" is " ++ (show result'))

  -- the end 
  return ()


partition :: Int -> [a] -> [[a]]
partition = partition2 []

partition2 :: [[a]] -> Int -> [a] -> [[a]]
partition2 lss n [] = lss
partition2 lss n ls = partition2 (left:lss) n right
  where
  (left,right) = splitAt n ls

      
merge :: [FS.FileId] -> FS.FileSystem -> IO [(Integer,Integer)]
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

instance Hash Integer where
  hash n k = mod (fromIntegral k) n

{-

type MapFunction a k1 v1 k2 v2 = ActionEnvironment -> a -> k1 -> v1 -> IO [(k2, v2)]
-}
countMap :: MapFunction [String] Integer [String] Integer Integer
countMap _env wordsToCount k1 v1 = do
--  putStrLn $ "map: ("++show k1++" / "++(show . length $ v1)++" )"
--  let result =  zip (repeat k1) (map counts v1)
--  return $ rnf result `seq` result
--  let c = counts v1
--  return [(c,c)]
  let cs= map (\x -> let x' = counts x in (k1,x')) v1 in rnf cs `seq` return cs 
  where
  counts :: String -> Integer
  counts word = (b2int . or . map (==word)) wordsToCount
  b2int True  = 1
  b2int False = 0

{-

type ReduceFunction a k2 v2 v3 = ActionEnvironment -> a -> k2 -> [v2] -> IO (Maybe v3)
-}
countReduce :: ReduceFunction [String]  Integer Integer Integer
countReduce _env _opts _k2 v2s = let b = Just . sum $ v2s in rnf b `seq` return b
