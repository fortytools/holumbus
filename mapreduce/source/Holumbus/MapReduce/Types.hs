-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.MapReduce.Types
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Holumbus.MapReduce.Types
(
  Hash(..)
, hashedPartition  
,  FunctionData(..)
  
{-
, encodeTuple
, decodeTuple
, decodeTupleList
, encodeTupleList
-}

-- * TaskData
, TaskId
, TaskType(..)
, TaskState(..)
, getNextTaskState
, TaskOutputType(..)
, TaskData(..)

-- * JobData
, JobId
, JobState(..)
, getNextJobState
, getPrevJobState
, fromJobStatetoTaskType
, OutputMap
, JobAction(..)
, JobInfo(..)
, JobData(..)
, JobResultContainer(..)
, JobResult(..)


-- * TaskAction
, ActionName
, ActionInfo

, ActionEnvironment(..)
, mkActionEnvironment

, InputReader
, OutputWriter

, OptionsDecoder


-- ----------------------------------------------------------------------------
-- remove this, when work done!
-- defaultFunctions (only for profiling)
, defaultInputReader
, defaultOutputWriter
, defaultSplit
, defaultPartition
, defaultMerge
, readConnector
, writeConnector
-- ----------------------------------------------------------------------------

, ActionConfiguration(..)
, SplitConfiguration(..)
, MapConfiguration(..)
, ReduceConfiguration(..)
, defaultActionConfiguration
, defaultSplitConfiguration
, defaultMapConfiguration
, defaultReduceConfiguration
, readActionConfiguration
, createJobInfoFromConfiguration
, createListsFromJobResult

, ActionData(..)
, getActionForTaskType
, ActionMap

-- * MapAction
, MapAction
-- , BinaryMapAction 
, MapFunction
, MapPartition

-- * Combine-Reduce-Action
, ReduceAction
-- , BinaryReduceAction
, ReduceMerge 
, ReduceFunction
, ReducePartition
, SplitFunction
, SplitAction
)
where

import           Control.Concurrent
import           Data.Binary
--import           Holumbus.Common.MRBinary
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.HashTable as Hash
import           Data.Maybe
import           Data.Time

import           System.Log.Logger
import           Data.Time.Clock.POSIX

import           Text.XML.HXT.Arrow

import qualified Holumbus.Data.AccuMap as AMap
import           Holumbus.Common.FileHandling
import           Holumbus.Common.Utils
import qualified Holumbus.Data.AccuMap as AMap
import qualified Holumbus.Data.KeyMap as KMap
import qualified Holumbus.FileSystem.FileSystem as FS
import           Control.Parallel.Strategies

localLogger :: String
localLogger = "Holumbus.MapReduce.Types"

-- ----------------------------------------------------------------------------
-- general datatypes
-- ----------------------------------------------------------------------------
class Hash a where
  hash :: Int -> a -> Int
  
instance Hash Int where
  hash n k = mod k n

data FunctionData
  = TupleFunctionData B.ByteString
  | FileFunctionData FS.FileId
  deriving (Show, Eq, Ord)

instance Binary FunctionData where
  put (TupleFunctionData t)  = putWord8 0 >> put t
  put (FileFunctionData f) = putWord8 1 >> put f
  get
    = do
      t <- getWord8
      case t of
        1 -> get >>= \f -> return (FileFunctionData f)
        _ -> get >>= \b -> return (TupleFunctionData b)
        
        
instance XmlPickler FunctionData where
  xpickle = xpFunctionData
        
xpFunctionData :: PU FunctionData
xpFunctionData
  = xpElem "data" $
      xpAlt tag ps
      where
      tag (TupleFunctionData _) = 0
      tag (FileFunctionData _)  = 1
      ps = [xpWrap (\p -> TupleFunctionData (encodeTuple p), \(TupleFunctionData p) -> decodeTuple p) xpRawFunctionData
           ,xpWrap (\f -> FileFunctionData f, \(FileFunctionData f) -> f) xpFileFunctionData ]
      xpRawFunctionData = xpPair (xpElem "key" xpText0) (xpElem "value" xpText0)
      xpFileFunctionData = xpElem "filename" xpText


        
-- ----------------------------------------------------------------------------
-- general encoding / decoding
-- ----------------------------------------------------------------------------

encodeTuple :: (Binary k, Binary v) => (k, v) -> B.ByteString
encodeTuple t = encode t


decodeTuple :: (Binary k, Binary v) => B.ByteString -> (k, v)
decodeTuple t = decode t


-- encodeTupleList :: (Binary k, Binary v) => [(k, v)] -> [B.ByteString]
-- encodeTupleList ls = map encodeTuple ls


-- decodeTupleList :: (Binary k, Binary v) => [B.ByteString] -> [(k, v)]
-- decodeTupleList ls = map decodeTuple ls




-- ----------------------------------------------------------------------------
-- Task DataTypes
-- ----------------------------------------------------------------------------


-- | the task id (should be unique in the system)
type TaskId = Integer


-- | which type (map, combine, reduce)
data TaskType = TTSplit | TTMap | TTCombine | TTReduce | TTError
  deriving (Show, Eq, Ord)

instance Binary TaskType where
  put (TTSplit)   = putWord8 1
  put (TTMap)     = putWord8 2
  put (TTCombine) = putWord8 3
  put (TTReduce)  = putWord8 4
  put (TTError)   = putWord8 0
  get
    = do
      t <- getWord8
      case t of
       1 -> return (TTSplit)
       2 -> return (TTMap)
       3 -> return (TTCombine)
       4 -> return (TTReduce)
       _ -> return (TTError)


-- | the task state
data TaskState = TSIdle | TSSending | TSInProgress | TSCompleted | TSFinished | TSError
  deriving (Show, Eq, Ord, Enum)

instance Binary TaskState where
  put (TSIdle)       = putWord8 1
  put (TSSending)    = putWord8 2
  put (TSInProgress) = putWord8 3
  put (TSCompleted)  = putWord8 4
  put (TSFinished)   = putWord8 5
  put (TSError)      = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> return (TSIdle)
        2 -> return (TSSending)
        3 -> return (TSInProgress)
        4 -> return (TSCompleted)
        5 -> return (TSFinished)
        _ -> return (TSError)

getNextTaskState :: TaskState -> TaskState
getNextTaskState TSError    = TSError
getNextTaskState TSFinished = TSFinished
getNextTaskState s          = succ s


data TaskOutputType = TOTRawTuple | TOTFile
  deriving (Show, Read, Eq, Ord, Enum)
  
instance Binary TaskOutputType where
  put (TOTRawTuple) = putWord8 0
  put (TOTFile)     = putWord8 1
  get
    = do
     t <- getWord8
     case t of
       0 -> return (TOTRawTuple)
       _ -> return (TOTFile)

instance XmlPickler TaskOutputType where
  xpickle = xpAttr "output" $ xpPrim


-- | the TaskData, contains all information to do the task
data TaskData = TaskData {
    td_JobId        :: ! JobId
  , td_TaskId       :: ! TaskId
  , td_Type         :: ! TaskType
  , td_State        :: ! TaskState
  , td_Option       :: ! B.ByteString
  , td_PartValue    :: ! (Maybe Int) 
  , td_Input        :: ! (Int, [FunctionData])
  , td_Output       :: ! [(Int,[FunctionData])]
  , td_OutputType   :: ! TaskOutputType
  , td_Action       :: ! ActionName
  } deriving (Show, Eq, Ord)

instance Binary TaskData where
  put (TaskData jid tid tt ts opt pv i o ot a)
    = put jid >> put tid >> put tt >> put ts >> put opt >> put pv >> put i >> put o >> put ot >> put a
  get
    = do
      jid <- get
      tid <- get
      tt  <- get
      ts  <- get
      opt <- get
      pv  <- get
      i   <- get
      o   <- get
      ot  <- get
      a   <- get
      return (TaskData jid tid tt ts opt pv i o ot a)




-- ----------------------------------------------------------------------------
-- Job Datatypes
-- ----------------------------------------------------------------------------


-- | the job id (should be unique in the system)
type JobId = Integer


-- | the job state
data JobState = JSPlanned | JSIdle | JSSplit | JSMap | JSCombine | JSReduce | JSCompleted | JSFinished | JSError
  deriving(Show, Eq, Ord, Enum)

instance Binary JobState where
  put (JSPlanned)    = putWord8 1
  put (JSIdle)       = putWord8 2
  put (JSSplit)      = putWord8 3
  put (JSMap)        = putWord8 4
  put (JSCombine)    = putWord8 5
  put (JSReduce)     = putWord8 6
  put (JSCompleted)  = putWord8 7
  put (JSFinished)   = putWord8 8
  put (JSError)      = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> return (JSPlanned)
        2 -> return (JSIdle)
        3 -> return (JSSplit)
        4 -> return (JSMap)
        5 -> return (JSCombine)
        6 -> return (JSReduce)
        7 -> return (JSCompleted)
        8 -> return (JSFinished)
        _ -> return (JSError)

getNextJobState :: JobState -> JobState
getNextJobState JSError    = JSError
getNextJobState JSFinished = JSFinished
getNextJobState s          = succ s

getPrevJobState :: JobState -> JobState
getPrevJobState JSIdle  = JSIdle
getPrevJobState JSError = JSError
getPrevJobState s       = pred s

fromJobStatetoTaskType :: JobState -> Maybe TaskType
fromJobStatetoTaskType JSSplit   = Just TTSplit
fromJobStatetoTaskType JSMap     = Just TTMap
fromJobStatetoTaskType JSCombine = Just TTCombine
fromJobStatetoTaskType JSReduce  = Just TTReduce
fromJobStatetoTaskType _         = Nothing



type OutputMap = Map.Map JobState (AMap.AccuMap Int FunctionData)

data JobAction = JobAction {
    ja_Name   :: ! ActionName
  , ja_Output :: ! TaskOutputType
  , ja_Count  :: ! Int
  } deriving (Show, Eq)

instance Binary JobAction where
  put (JobAction n o c) = put n >> put o >> put c
  get 
    = do
      n <- get
      o <- get
      c <- get
      return (JobAction n o c)

instance XmlPickler JobAction where
  xpickle = xpJobAction
  
xpJobAction :: PU JobAction
xpJobAction =
  xpWrap
    (\(n, o, c) -> JobAction n o c, 
     \(JobAction n o c) -> (n, o, c)) xpAction
  where
  xpAction = xpTriple (xpFunction) (xpickle) (xpCount)
  xpFunction = xpAttr "name" xpText
  xpCount = xpWrap ((maybe 1 id), Just) $ xpOption $ xpAttr "count" xpickle
      


-- | defines a job, this is all data the user has to give to run a job
data JobInfo = JobInfo {
    ji_Description       :: ! String
  , ji_Option            :: ! B.ByteString
  , ji_SplitAction       :: ! (Maybe JobAction)
  , ji_MapAction         :: ! (Maybe JobAction)
  , ji_CombineAction     :: ! (Maybe JobAction)
  , ji_ReduceAction      :: ! (Maybe JobAction)
  , ji_NumOfResults      :: ! (Maybe Int)
  , ji_Input             :: ! [FunctionData]
  } deriving (Show, Eq)

instance Binary JobInfo where
  put (JobInfo d opt sa ma ca ra nor i)
    = put d >> put opt >> 
      put sa >> put ma >> put ca >> put ra >> put nor >> put i
  get
    = do
      d   <- get
      opt <- get
      sa  <- get
      ma  <- get
      ca  <- get
      ra  <- get
      nor  <- get
      i   <- get
      return (JobInfo d opt sa ma ca ra nor i)
   
instance XmlPickler JobInfo where
  xpickle = xpJobInfo
  
xpJobInfo :: PU JobInfo
xpJobInfo = 
    xpElem "jobInfo" $
    xpWrap
      (\(n, (sa,ma,ca,ra), nor, i) -> JobInfo n (encode ()) sa ma ca ra nor i, 
       \(JobInfo n _ sa ma ca ra nor i) -> (n, (sa,ma,ca,ra), nor, i)) xpJob
    where
    xpJob = 
      xp4Tuple
        (xpAttr "name" xpText0)
        (xpActions)
        (xpCount)
        (xpFunctionDataList)
    xpActions = xp4Tuple (xpSplitAction) (xpMapAction) (xpCombineAction) (xpReduceAction)
    xpSplitAction   = xpOption $ xpElem "split" $ xpJobAction
    xpMapAction     = xpOption $ xpElem "map" $ xpJobAction
    xpCombineAction = xpOption $ xpElem "combine" $ xpJobAction
    xpReduceAction  = xpOption $ xpElem "reduce" $ xpJobAction
    xpFunctionDataList
      = xpWrap (filterEmptyList,setEmptyList) $ xpOption $
        xpElem "inputList" $ xpList xpFunctionData
    xpCount = xpOption $ xpAttr "numOfResults" xpickle
    

-- | the job data, include the user-input and some additional control-data
data JobData = JobData {
    jd_JobId       :: JobId
  , jd_State       :: JobState
  , jd_OutputMap   :: OutputMap
  , jd_Info        :: JobInfo
  , jd_startTime   :: UTCTime
  , jd_endTime     :: UTCTime
  , jd_Result      :: JobResultContainer
  }

instance Show JobData where
  show (JobData jid state om _ t1 t2 _) =
    "JobId:\t" ++ show jid ++ "\n" ++
    "State:\t" ++ show state ++ "\n" ++
    "Info:\tJobInfo\n" ++
    "StartTime:\t" ++  show t1 ++ "\n" ++
    "EndTime:\t" ++ show t2 ++ "\n" ++
    "OutputMap:\n" ++ show om ++ "\n"


data JobResultContainer = JobResultContainer (MVar JobResult)

instance Show JobResultContainer where
  show _ = "JobResultContainer"

-- | the result of the job, given by the master
data JobResult = JobResult {
    jr_Output        :: [FunctionData]
  } deriving (Show)

instance Binary JobResult where
  put (JobResult o) = put o
  get
    = do
      o <- get
      return (JobResult o)



-- ----------------------------------------------------------------------------
-- Reader and Writer
-- ----------------------------------------------------------------------------

-- | the ActionEnvironment contains all data that might be needed
--   during an action. So far, it only keeps the current task data and
--   a reference to the global filesystem and the options. 
--   This is a good place to implement counters for the map-reduce-system 
--   or other stuff...
data ActionEnvironment = ActionEnvironment {
    ae_TaskData   :: TaskData
  , ae_FileSystem :: FS.FileSystem
  }

mkActionEnvironment :: TaskData -> FS.FileSystem -> ActionEnvironment
mkActionEnvironment td fs = ActionEnvironment td fs


type InputReader k1 v1 = B.ByteString -> IO [(k1,v1)]

type OutputWriter k2 v2 = [(k2,v2)] -> IO B.ByteString

readConnector
  :: (NFData k1, NFData v1, Binary k1, Binary v1)
  => InputReader k1 v1
  -> ActionEnvironment
  -> [FunctionData] 
  -> IO [(k1,v1)]
readConnector ic ae ls
  = do
    os <- mapM (readInput (ae_FileSystem ae)) ls
    return . concat . catMaybes $ os
    where    
    -- readInput :: FS.FileSystem -> FunctionData -> IO (Maybe [(k1,v1)])
    readInput _  (TupleFunctionData t) = return $ Just $ [decode t] 
    readInput fs (FileFunctionData f)
      = do        
        debugM localLogger $ "loadInputList: getting content for: " ++ f
        mbc <- FS.getFileContent f fs
        if isNothing mbc
          then do
            debugM localLogger $ "loadInputList: no content found"
            return Nothing
          else do
            d <- ic $ fromJust mbc
            return $ Just d
--
--myMapM :: (a -> IO b) -> [a] -> IO [b]
--myMapM f [] = return []
--myMapM f (x:xs) = do
--   infoM localLogger $ "myMapM: do f"
--   b <- f x
--   infoM localLogger $ "myMapM: do recursiv map"
--   bs <- myMapM f xs
--   infoM localLogger $ "myMapM: do return"
--   return (b:bs)
   
     

writeConnector 
  :: (Binary k2, Binary v2)
  => OutputWriter k2 v2
  -> ActionEnvironment
  -> [(Int,[(k2,v2)])] 
  -> IO [(Int,[FunctionData])]
writeConnector oc ae ls
  = do
    infoM localLogger $ "writeConnector: " ++ (show . length $ ls)
    os <- case tot of
      TOTRawTuple -> mapM (writeOutput (ae_FileSystem ae) tot) ls
      _           -> write2
    return $ catMaybes os
    where 

    write2 = do
      bincontents <- mapM (\(_,c) -> oc c) ls
      let filelist = zip filenames bincontents;
          
      FS.createFiles filelist fs
      return $ (zipWith (\(i,_) fn -> Just (i,[FileFunctionData fn]) ) ls filenames)
      where
      filenames = map (\(i,_) -> "j" ++ show (td_JobId td) ++ "_t" ++ show (td_TaskId td) ++ "_i" ++ show i) ls
      fs = ae_FileSystem ae
 
    td   = ae_TaskData ae
    tot  = td_OutputType $ ae_TaskData ae
    -- writeOutput :: FS.FileSystem -> TaskOutputType -> (Int,[(k2,v2)]) -> IO (Maybe (Int,[FunctionData]))
    writeOutput _  TOTRawTuple (i,ts) 
      = return $ Just $ (i,bs)
      where
      bs = map (\t -> TupleFunctionData $ encode t) ts
    -- TODO exception werfen 
    writeOutput fs _ (i,ts)
      = do
        infoM localLogger "oc ts"
        c <-  oc ts        
        infoM localLogger "appendfile"
        FS.appendFile fn c fs
        infoM localLogger "return just"
        return $ Just (i,[FileFunctionData fn])
        where
        fn =  "j" ++ show (td_JobId td) ++ "_t" ++ show (td_TaskId td) ++ "_i" ++ show i


defaultInputReader :: (NFData v1, NFData k1, Binary k1, Binary v1) => InputReader k1 v1
defaultInputReader = return . parseByteStringToList


defaultOutputWriter :: (NFData v2, NFData k2, Binary k2, Binary v2) => OutputWriter k2 v2
defaultOutputWriter = return . listToByteString 
--  = return $ B.concat $ map encode ls


type ActionName = String

type ActionInfo = String

data ActionConfiguration a k1 v1 k2 v2 v3 v4
  = ActionConfiguration {
    ac_Name          :: ActionName
  , ac_Info          :: ActionInfo
  , ac_OptEncoder    :: OptionsEncoder a
  , ac_OptDecoder    :: OptionsDecoder a
  , ac_InputEncoder  :: InputEncoder k1 v1
  , ac_OutputDecoder :: OutputDecoder k2 v4
  , ac_Split         :: Maybe (SplitConfiguration a k1 v1)
  , ac_Map           :: Maybe (MapConfiguration a k1 v1 k2 v2)
  , ac_Combine       :: Maybe (ReduceConfiguration a k2 v2 v3)
  , ac_Reduce        :: Maybe (ReduceConfiguration a k2 v3 v4)
  }  


data SplitConfiguration a k1 v1
  = SplitConfiguration {
    sc_Function :: SplitFunction a k1 v1
  , sc_Reader   :: InputReader k1 v1
  , sc_Writer   :: OutputWriter k1 v1
  }


data MapConfiguration a k1 v1 k2 v2
  = MapConfiguration {
    mc_Function  :: MapFunction a k1 v1 k2 v2
  , mc_Partition :: MapPartition a k2 v2
  , mc_Reader    :: InputReader k1 v1
  , mc_Writer    :: OutputWriter k2 v2
  }


data ReduceConfiguration a k2 v3 v4
  = ReduceConfiguration {
    rc_Merge     :: ReduceMerge a k2 v3
  , rc_Function  :: ReduceFunction a k2 v3 v4
  , rc_Partition :: ReducePartition a k2 v4
  , rc_Reader    :: InputReader k2 v3
  , rc_Writer    :: OutputWriter k2 v4
  }


defaultActionConfiguration
  :: (NFData v1, NFData k1, Binary a, Binary k1, Binary v1, Binary k2, Binary v4)
  => ActionName
  -> ActionConfiguration a k1 v1 k2 v2 v3 v4
defaultActionConfiguration name
  = ActionConfiguration
      name
      ""
      defaultOptionsEncoder
      defaultOptionsDecoder
      defaultInputEncoder
      defaultOutputDecoder
      (Just defaultSplitConfiguration)
      Nothing
      Nothing
      Nothing


defaultSplitConfiguration
  :: (NFData v1, NFData k1, Binary a, Binary k1, Binary v1)
  => SplitConfiguration a k1 v1
defaultSplitConfiguration 
  = SplitConfiguration
      defaultSplit
      defaultInputReader
      defaultOutputWriter 


defaultMapConfiguration
  :: (NFData v1, NFData k1, NFData v2, NFData k2, Ord k2, Binary a, Binary k1, Binary v1, Binary k2, Binary v2)
  => MapFunction a k1 v1 k2 v2
  -> MapConfiguration a k1 v1 k2 v2
defaultMapConfiguration fct
  = MapConfiguration
      fct
      defaultPartition
      defaultInputReader
      defaultOutputWriter


defaultReduceConfiguration
  :: (NFData v2, NFData k2, NFData v3, Ord k2, Binary a, Binary k2, Binary v2, Binary v3)
  => ReduceFunction a k2 v2 v3
  -> ReduceConfiguration a k2 v2 v3
defaultReduceConfiguration fct
  = ReduceConfiguration
      defaultMerge
      fct
      defaultPartition
      defaultInputReader
      defaultOutputWriter


defaultSplit
  :: (NFData k1, NFData v1) => SplitFunction a k1 v1
defaultSplit _ _ n ls = do
  infoM localLogger "defaultSplit"
  return partedList
    where
    partedList = AMap.toList $ AMap.fromList ps
    ns = [(x `mod` n) + 1 | x <- [0..]]
    is = map (\a -> [a]) ls 
    ps = zip ns is


hashedPartition :: (Hash k2, Binary k2, Binary v2, NFData k2, NFData v2) => MapPartition a k2 v2
hashedPartition _ _ 1 l = return . (:[]) . (,) 1 $ l
hashedPartition _ _ n l = do 
  infoM localLogger "hashedPartition: map"
  let a =  map (\t -> (hash n (fst t),[t])) l
  infoM localLogger "hashedPartition: fromList"
  let b = AMap.fromList a
  infoM localLogger "hashedPartition: toList"
  let c = AMap.toList b
  infoM localLogger "hashedPartition: return"
  return $ c

defaultPartition
  :: (Binary k2, Binary v2)
  => MapPartition a k2 v2
defaultPartition _ _ 1 ls
  -- To make it faster, wenn no partition is used
  = return [(1,ls)]
defaultPartition _ _ n ls
  = do
    -- calculate partition-Values
    let markedList = map (\t@(k,_) -> (hash' k,t)) ls
    -- merge them
    
    -- TODO this might change (revert) the order of the Elements...
    let resultList = AMap.toList $ AMap.fromTupleList markedList
    return resultList
    where
    -- calculate a hash-value, because we only have the Binary-Instance, we
    -- can only use the Bytestring of the Value
    hash' k = ((fromIntegral $ Hash.hashString (show $ encode k)) `mod` n) + 1


defaultMerge
  :: (Ord k2, Binary k2, Binary v2)
  => ReduceMerge a k2 v2
defaultMerge _ _ ls
  = return $ AMap.toList $ AMap.fromTupleList ls



data ActionData
  = ActionData {
    ad_Name    :: ActionName
  , ad_Info    :: ActionInfo
  , ad_Split   :: Maybe BinarySplitAction
  , ad_Map     :: Maybe BinaryMapAction
  , ad_Combine :: Maybe BinaryReduceAction
  , ad_Reduce  :: Maybe BinaryReduceAction
  }

instance KMap.Key ActionData where
  getKey = ad_Name

instance Show ActionData where
  show (ActionData n i _ _ _ _) = "{ActionData name:\"" ++ n ++ "\" info:\"" ++ i ++ "\"}"


type ActionMap = KMap.KeyMap ActionData

type OptionsEncoder a = a -> B.ByteString
type OptionsDecoder a = B.ByteString -> a

defaultOptionsEncoder :: (Binary a) => OptionsEncoder a
defaultOptionsEncoder = encode

defaultOptionsDecoder :: (Binary a) => OptionsDecoder a
defaultOptionsDecoder = decode


type InputEncoder k1 v1 = [(k1,v1)] -> [FS.FileId] -> [FunctionData]
type OutputDecoder k2 v4 = [FunctionData] -> ([(k2,v4)],[FS.FileId])

defaultInputEncoder :: (Binary k1, Binary v1) => InputEncoder k1 v1
defaultInputEncoder ls1 ls2 = ls1' ++ ls2'
  where
  ls1' = map (\t -> TupleFunctionData (encode t)) ls1
  ls2' = map (\f -> FileFunctionData f) ls2

defaultOutputDecoder :: (Binary k2, Binary v4) => OutputDecoder k2 v4
defaultOutputDecoder ls = (ls1, ls2)
  where
  ls1 = map (\t -> decode t) $ catMaybes ls1'
  ls2 = catMaybes ls2'
  (ls1',ls2') = unzip $ map (splitter) ls
  splitter (TupleFunctionData t) = (Just t, Nothing)
  splitter (FileFunctionData f)  = (Nothing, Just f)
  

getActionForTaskType :: TaskType -> ActionData -> Maybe BinaryReduceAction
getActionForTaskType TTSplit   ad = ad_Split ad
getActionForTaskType TTMap     ad = ad_Map ad
getActionForTaskType TTCombine ad = ad_Combine ad
getActionForTaskType TTReduce  ad = ad_Reduce ad
getActionForTaskType _         _  = Nothing

  

readActionConfiguration
  :: ( Ord k2, Binary a
     , Show k1, Show v1
     , Show k2, Show v2, Show v3, Show v4
     , NFData k1, NFData v1
     , NFData k2, NFData v2, NFData v3
     , Binary k1, Binary v1
     , Binary k2, Binary v2
     , Binary v3, Binary v4)
  => ActionConfiguration a k1 v1 k2 v2 v3 v4 
  -> ActionData
readActionConfiguration (ActionConfiguration n i _ optDec _ _ sc mc cc rc)
  = ActionData n i sf mf cf rf
  where
  sf = maybe Nothing (\(SplitConfiguration f ir ow) -> Just $ performSplitAction optDec f ir ow) sc 
  mf = maybe Nothing (\(MapConfiguration f p ir ow) -> Just $ performMapAction optDec f p ir ow) mc
  cf = maybe Nothing (\(ReduceConfiguration m f p ir ow) -> Just $ performReduceAction optDec m f p ir ow) cc
  rf = maybe Nothing (\(ReduceConfiguration m f p ir ow) -> Just $ performReduceAction optDec m f p ir ow) rc



createJobInfoFromConfiguration
  :: ActionConfiguration a k1 v1 k2 v2 v3 v4
  -> a              -- ^ options
  -> [(k1,v1)]      -- ^ input (Tuples)
  -> [FS.FileId]    -- ^ input (Files)
  -> Int            -- ^ number of splitters
  -> Int            -- ^ number of mappers
  -> Int            -- ^ number of reducers
  -> Int            -- ^ number of results
  -> TaskOutputType -- ^ type of the result (file of raw)
  -> JobInfo
createJobInfoFromConfiguration (ActionConfiguration n _ optEnc _ inEnc _ sConf mConf cConf rConf) opts' ls1 ls2 sCnt mCnt rCnt nor' rt
  = JobInfo n opts sa ma ca ra nor i
  where
  opts = optEnc opts'
  sa   = maybe Nothing (\_ -> Just $ JobAction n TOTFile sCnt) sConf
  ma   = maybe Nothing (\_ -> Just $ JobAction n TOTFile mCnt) mConf
  ca   = maybe Nothing (\_ -> Just $ JobAction n TOTFile mCnt) cConf
  ra   = maybe Nothing (\_ -> Just $ JobAction n rt      rCnt) rConf
  nor  = Just nor'
  i    = inEnc ls1 ls2
  
    
createListsFromJobResult
  :: ActionConfiguration a k1 v1 k2 v2 v3 v4
  -> JobResult
  -> ([(k2,v4)],[FS.FileId])
createListsFromJobResult ac jr = (ac_OutputDecoder ac) (jr_Output jr)
    
-- ----------------------------------------------------------------------------
-- SplitAction
-- ----------------------------------------------------------------------------

-- | general SplitAction
type SplitAction a k1 v1 = ActionEnvironment -> a -> Int -> [(k1,v1)] -> IO [(Int, [(k1,v1)])]

-- | SplitAction on ByteStrings
type BinarySplitAction = ActionEnvironment -> B.ByteString -> Maybe Int -> (Int,[FunctionData]) -> IO [(Int, [FunctionData])] 

type SplitFunction a k1 v1 = SplitAction a k1 v1

performSplitAction
  :: (NFData k1, NFData v1, Binary a, Binary k1, Binary v1, Show k1, Show v1)
  => OptionsDecoder a
  -> SplitFunction a k1 v1
  -> InputReader k1 v1
  -> OutputWriter k1 v1
  -> ActionEnvironment
  -> B.ByteString
  -> Maybe Int
  -> (Int,[FunctionData])
  -> IO [(Int, [FunctionData])]
performSplitAction optDec fct reader writer env opts n (i,ls)
  = do
    let a = optDec opts
    
    infoM localLogger "performSplitAction"
    putTimeStamp "Begin performSplitAction"
    
    infoM localLogger "reading inputList"
    inputList <- readConnector reader env ls
    debugM localLogger $ ">>>>>>>>>>>>>>>>>>  input is: " ++ show inputList ++ "\n\n"
    infoM localLogger "doing split"
    partedList <- case n of
      (Just n') -> fct env a n' inputList
      (Nothing) -> return [(i,inputList)]
    
    debugM localLogger $ ">>>>>>>>>>>>>>>>>> splittet list is: " ++ show partedList ++ "\n\n"
    infoM localLogger "writing outputlist"
    outputList <- writeConnector writer env partedList
    putTimeStamp "End performSplitAction"
    return outputList


-- ----------------------------------------------------------------------------
-- MapAction
-- ----------------------------------------------------------------------------


-- | general MapAction
type MapAction a k1 v1 k2 v2 = ActionEnvironment -> a -> Int -> [(k1,v1)] -> IO [(Int, [(k2,v2)])]

-- | MapAction on ByteStrings
type BinaryMapAction = ActionEnvironment -> B.ByteString -> Maybe Int -> (Int,[FunctionData]) -> IO [(Int, [FunctionData])] 

type MapFunction a k1 v1 k2 v2 = ActionEnvironment -> a -> k1 -> v1 -> IO [(k2, v2)]

type MapPartition a k2 v2 = ActionEnvironment -> a -> Int -> [(k2,v2)] -> IO [(Int, [(k2,v2)])]
    

performMapAction
  :: (Ord k2, Show k1, Show k2, Show v1, Show v2,
      Binary a, Binary k1, Binary v1, Binary k2, Binary v2, NFData k2, NFData v2, NFData v1, NFData k1)
  => OptionsDecoder a
  -> MapFunction a k1 v1 k2 v2
  -> MapPartition a k2 v2
  -> InputReader k1 v1
  -> OutputWriter k2 v2
  -> ActionEnvironment
  -> B.ByteString
  -> Maybe Int
  -> (Int,[FunctionData])
  -> IO [(Int, [FunctionData])]
performMapAction optDec fct part reader writer env opts n (i,ls)
  = do
    -- decode the options
    let a = optDec opts
    
    infoM localLogger "performMapAction"
    putTimeStamp "Begin performMapAction"
    
    infoM localLogger "reading inputList"
    inputList <- readConnector reader env ls
    debugM localLogger $ ">>>>>>>>>>>>>>>>>>  input is: " ++ show inputList ++ "\n\n"
        
    infoM localLogger "doing map"
    mappedList <- mapM (\(k1, v1) -> fct env a k1 v1) inputList
    let tupleList = concat mappedList

    debugM localLogger $ ">>>>>>>>>>>>>>>>>>  mapped list is: " ++ show tupleList ++ "\n\n"

    infoM localLogger "doing partition"
    partedList <- case n of
      (Just n') -> part env a n' tupleList
      (Nothing) -> return [(i,tupleList)]
    
    debugM localLogger $ ">>>>>>>>>>>>>>>>>>  partitioned list is: " ++ show partedList ++ "\n\n"
    
    infoM localLogger "writing outputlist: begin"
    outputList <- writeConnector writer env partedList
    infoM localLogger "writing outputlist: done"    
    putTimeStamp "End performMapAction"
    return outputList





-- ----------------------------------------------------------------------------
-- Combine- / ReduceTask
-- ----------------------------------------------------------------------------

-- | general MapAction
type ReduceAction a k2 v2 v3 = ActionEnvironment -> a -> Int -> [(k2,v2)] -> IO [(Int, [(k2,v3)])]

-- | MapAction on ByteStrings
type BinaryReduceAction = ActionEnvironment -> B.ByteString -> Maybe Int -> (Int,[FunctionData]) -> IO [(Int, [FunctionData])] 

type ReduceMerge a k2 v2 = ActionEnvironment -> a -> [(k2,v2)] -> IO [(k2,[v2])]

type ReduceFunction a k2 v2 v3 = ActionEnvironment -> a -> k2 -> [v2] -> IO (Maybe v3)

type ReducePartition a k2 v3 = ActionEnvironment -> a -> Int -> [(k2,v3)] -> IO [(Int, [(k2,v3)])]
    

performReduceAction
  :: (Ord k2, Show k2, Show v2, Show v3,
      NFData k2, NFData v2,
      Binary a, Binary k2, Binary v2, Binary v3)
  => OptionsDecoder a
  -> ReduceMerge a k2 v2
  -> ReduceFunction a k2 v2 v3
  -> ReducePartition a k2 v3
  -> InputReader k2 v2
  -> OutputWriter k2 v3
  -> ActionEnvironment
  -> B.ByteString
  -> Maybe Int
  -> (Int,[FunctionData])
  -> IO [(Int, [FunctionData])]
performReduceAction optDec merge fct part reader writer env opts n (i,ls)
  = do
    -- decode the options
    let a = optDec opts
  
    infoM localLogger "performReduceAction"
    putTimeStamp "Begin performReduceAction"
    
    infoM localLogger "reading inputList"
    inputList <- readConnector reader env ls
    infoM localLogger $ ">>>>>>>>>>>>>>>>>>  input is: " ++ (show . length $ inputList) ++ "\n\n"
    debugM localLogger $ ">>>>>>>>>>>>>>>>>>  input is: " ++ show inputList ++ "\n\n"
        
    infoM localLogger "doing merge"
    mergedList <- merge env a inputList
    debugM localLogger $ ">>>>>>>>>>>>>>>>>>  mergedList is: " ++ show mergedList     ++ "\n\n"
    
    infoM localLogger "doing reduce"
    maybesList <- mapM (\(k2,v2s) -> performReduceFunction a k2 v2s) mergedList
    let tupleList = catMaybes maybesList
    debugM localLogger $ ">>>>>>>>>>>>>>>>>>  tupleList is: " ++ show tupleList ++ "\n\n"
    
    infoM localLogger "doing partition" 
    partedList <- case n of
      (Just n') -> part env a n' tupleList
      (Nothing) -> return [(i,tupleList)] 

    debugM localLogger $ ">>>>>>>>>>>>>>>>>>  partedList is: " ++ show partedList ++ "\n\n"
    
    infoM localLogger "writing outputlist: begin"
    outputList <- writeConnector writer env partedList
    infoM localLogger "writing outputlist: done"
    
    putTimeStamp "End performReduceAction"
    return outputList
    where
      performReduceFunction a k2 v2s
        = do
          mbV3 <- fct env a k2 v2s
          case mbV3 of
            (Nothing) -> return Nothing
            (Just v3) -> return $ Just (k2,v3)

putTimeStamp :: String -> IO ()
putTimeStamp s = do
  t1 <- getPOSIXTime
  infoM localLogger (s++" : "++ show t1)
