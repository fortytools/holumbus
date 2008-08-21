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
  FunctionData(..)

, encodeTuple
, decodeTuple
, decodeTupleList
, encodeTupleList

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
, JobInfo(..)
, JobData(..)
, JobResultContainer(..)
, JobResult(..)


-- * TaskAction
, ActionName
, ActionInfo

, ActionEnvironment(..)
, mkActionEnvironment

, ActionConnector(..)
, defaultActionConnector

, OptionsDecoder

, ActionConfiguration(..)
, MapConfiguration(..)
, ReduceConfiguration(..)
, defaultActionConfiguration
, defaultMapConfiguration
, defaultReduceConfiguration
, readActionConfiguration

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
)
where

import           Control.Concurrent
import           Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.HashTable as Hash
import           Data.Maybe
import           Data.Time

import           System.Log.Logger

import           Text.XML.HXT.Arrow

import           Holumbus.Common.Utils
-- import           Holumbus.MapReduce.TypeCheck
import qualified Holumbus.Data.AccuMap as AMap
import qualified Holumbus.Data.KeyMap as KMap
import qualified Holumbus.FileSystem.FileSystem as FS


localLogger :: String
localLogger = "Holumbus.MapReduce.Types"

-- ----------------------------------------------------------------------------
-- general datatypes
-- ----------------------------------------------------------------------------


data FunctionData
  = TupleFunctionData B.ByteString
  | FileFunctionData String
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


encodeTupleList :: (Binary k, Binary v) => [(k, v)] -> [B.ByteString]
encodeTupleList ls = map encodeTuple ls


decodeTupleList :: (Binary k, Binary v) => [B.ByteString] -> [(k, v)]
decodeTupleList ls = map decodeTuple ls




-- ----------------------------------------------------------------------------
-- Task DataTypes
-- ----------------------------------------------------------------------------


-- | the task id (should be unique in the system)
type TaskId = Integer


-- | which type (map, combine, reduce)
data TaskType = TTMap | TTCombine | TTReduce | TTError
  deriving (Show, Eq, Ord)

instance Binary TaskType where
  put (TTMap)     = putWord8 1
  put (TTCombine) = putWord8 2
  put (TTReduce)  = putWord8 3
  put (TTError)   = putWord8 0
  get
    = do
      t <- getWord8
      case t of
       1 -> return (TTMap)
       2 -> return (TTCombine)
       3 -> return (TTReduce)
       _ -> return (TTError)


-- | the task state
data TaskState = TSIdle | TSInProgress | TSCompleted | TSFinished | TSError
  deriving (Show, Eq, Ord, Enum)

instance Binary TaskState where
  put (TSIdle)       = putWord8 1
  put (TSInProgress) = putWord8 2
  put (TSCompleted)  = putWord8 3
  put (TSFinished)   = putWord8 4
  put (TSError)      = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> return (TSIdle)
        2 -> return (TSInProgress)
        3 -> return (TSCompleted)
        4 -> return (TSFinished)
        _ -> return (TSError)

getNextTaskState :: TaskState -> TaskState
getNextTaskState TSError    = TSError
getNextTaskState TSFinished = TSFinished
getNextTaskState s          = succ s


data TaskOutputType = TOTRawTuple | TOTText | TOTList | TOTBin
  deriving (Show, Read, Eq, Ord, Enum)
  
instance Binary TaskOutputType where
  put (TOTRawTuple) = putWord8 0
  put (TOTText)     = putWord8 1
  put (TOTList)     = putWord8 2
  put (TOTBin)      = putWord8 3
  get
    = do
     t <- getWord8
     case t of
       0 -> return (TOTRawTuple)
       2 -> return (TOTList)
       3 -> return (TOTBin)
       _ -> return (TOTText)

instance XmlPickler TaskOutputType where
  xpickle = xpAttr "output" $ xpPrim


-- | the TaskData, contains all information to do the task
data TaskData = TaskData {
    td_JobId      :: ! JobId
  , td_TaskId     :: ! TaskId
  , td_Type       :: ! TaskType
  , td_State      :: ! TaskState
  , td_Option     :: ! B.ByteString
  , td_Parts      :: ! Int
  , td_Input      :: ! [FunctionData]
  , td_Output     :: ! [(Int,[FunctionData])]
  , td_OutputType :: ! TaskOutputType
  , td_Action     :: ! ActionName
  } deriving (Show, Eq, Ord)

instance Binary TaskData where
  put (TaskData jid tid tt ts opt n i o ot a)
    = put jid >> put tid >> put tt >> put ts >> put opt >> put n >> put i >> put o >> put ot >> put a
  get
    = do
      jid <- get
      tid <- get
      tt  <- get
      ts  <- get
      opt <- get
      n   <- get
      i   <- get
      o   <- get
      ot  <- get
      a   <- get
      return (TaskData jid tid tt ts opt n i o ot a)




-- ----------------------------------------------------------------------------
-- Job Datatypes
-- ----------------------------------------------------------------------------


-- | the job id (should be unique in the system)
type JobId = Integer


-- | the job state
data JobState = JSPlanned | JSIdle | JSMap | JSCombine | JSReduce | JSCompleted | JSFinished | JSError
  deriving(Show, Eq, Ord, Enum)

instance Binary JobState where
  put (JSPlanned)    = putWord8 1
  put (JSIdle)       = putWord8 2
  put (JSMap)        = putWord8 3
  put (JSCombine)    = putWord8 4
  put (JSReduce)     = putWord8 5
  put (JSCompleted)  = putWord8 6
  put (JSFinished)   = putWord8 7
  put (JSError)      = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> return (JSPlanned)
        2 -> return (JSIdle)
        3 -> return (JSMap)
        4 -> return (JSCombine)
        5 -> return (JSReduce)
        6 -> return (JSCompleted)
        7 -> return (JSFinished)
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
fromJobStatetoTaskType JSMap     = Just TTMap
fromJobStatetoTaskType JSCombine = Just TTCombine
fromJobStatetoTaskType JSReduce  = Just TTReduce
fromJobStatetoTaskType _         = Nothing



type OutputMap = Map.Map JobState (AMap.AccuMap Int FunctionData)


-- | defines a job, this is all data the user has to give to run a job
data JobInfo = JobInfo {
    ji_Description       :: ! String
  , ji_Option            :: ! B.ByteString
  , ji_MapAction         :: ! (Maybe ActionName)
  , ji_CombineAction     :: ! (Maybe ActionName)
  , ji_ReduceAction      :: ! (Maybe ActionName)
  , ji_MapOutputType     :: ! (Maybe TaskOutputType)
  , ji_CombineOutputType :: ! (Maybe TaskOutputType)
  , ji_ReduceOutputType  :: ! (Maybe TaskOutputType)
  , ji_MapPart           :: ! Int
  , ji_CombinePart       :: ! Int
  , ji_ReducePart        :: ! Int
  , ji_Input             :: ! [FunctionData]
  } deriving (Show, Eq)

instance Binary JobInfo where
  put (JobInfo d opt ma ca ra mo co ro mp cp rp i)
    = put d >> put opt >> 
      put ma >> put ca >> put ra >>
      put mo >> put co >> put ro >>
      put mp >> put cp >> put rp >>
      put i
  get
    = do
      d   <- get
      opt <- get
      ma  <- get
      ca  <- get
      ra  <- get
      mo  <- get
      co  <- get
      ro  <- get
      mp  <- get
      cp  <- get
      rp  <- get
      i   <- get
      return (JobInfo d opt ma ca ra mo co ro mp cp rp i)


instance XmlPickler JobInfo where
  xpickle = xpJobInfo
  
xpJobInfo :: PU JobInfo
xpJobInfo = 
    xpElem "jobInfo" $
    xpWrap
      (\(n, (ma,mo,mp), (ca,co,cp), (ra,ro,rp), i) -> JobInfo n (encode ()) ma ca ra mo co ro mp cp rp i, 
       \(JobInfo n _ ma ca ra mo co ro mp cp rp i) -> (n, (ma,mo,mp), (ca,co,cp), (ra,ro,rp), i)) xpJob
    where
    xpJob = 
      xp5Tuple
        (xpAttr "name" xpText0)
        (xpMapAction)
        (xpCombineAction)
        (xpReduceAction)
        (xpFunctionDataList)
      where 
      xpMapAction     = xpElem "map" $ xpTaskAction
      xpCombineAction = xpElem "combine" $ xpTaskAction
      xpReduceAction  = xpElem "reduce" $ xpTaskAction
      xpFunctionDataList
        = xpWrap (filterEmptyList,setEmptyList) $ xpOption $
          xpElem "inputList" $ xpList xpFunctionData
      xpFunction = xpOption $ xpAttr "name" xpText
      xpParts    = xpWrap ((maybe 1 id), Just) $ xpOption $ xpAttr "parts" xpickle
      xpTaskAction = xpTriple (xpFunction) (xpickle) (xpParts)
      

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
-- TestJobInfo
-- ----------------------------------------------------------------------------

{-
testJobInfo :: JobInfo -> MapActionMap -> ReduceActionMap -> (Bool, String)
testJobInfo ji mm rm = foldl (testAnd) (True, "") testList
  where
  testAnd t1@(b1, _) t2@(b2, _)
    | (b1 == True) && (b2 == True) = t1
    | (b1 == False)                = t1
    | otherwise                    = t2 
  testList  = [(testNames, "no actions given"),
               (testMap, "map-action not found"),
               (testCom, "combine-action not found"),
               (testRed, "reduce-action not found"),
               (testIOMC, "output of map and input of combine don't match"),
               (testIOCR, "output of combine and input of reduce don't match"),
               (testIOMR, "output of map and input of reduce don't match")]
  mapAction = ji_MapAction ji
  comAction = ji_CombineAction ji
  redAction = ji_ReduceAction ji
  mapName   = fromJust mapAction -- ta_Action $ fromJust mapAction
  comName   = fromJust comAction -- ta_Action $ fromJust comAction
  redName   = fromJust redAction -- ta_Action $ fromJust redAction
  mapOut    = mad_OutputType $ fromJust $ Map.lookup mapName mm
  comOut    = rad_OutputType $ fromJust $ Map.lookup comName rm
  comIn     = rad_InputType $ fromJust $ Map.lookup comName rm
  redIn     = rad_InputType $ fromJust $ Map.lookup redName rm
  testNames = (mapAction /= Nothing) || (comAction /= Nothing) || (redAction /= Nothing)
  testMap   = (mapAction == Nothing) || (isJust $ Map.lookup mapName mm)
  testCom   = (comAction == Nothing) || (isJust $ Map.lookup comName rm)
  testRed   = (redAction == Nothing) || (isJust $ Map.lookup redName rm)
  testIOMC  = (mapAction == Nothing) || (comAction == Nothing) || (mapOut == comIn)
  testIOCR  = (comAction == Nothing) || (redAction == Nothing) || (comOut == redIn)
  testIOMR  = (mapAction == Nothing) || (redAction == Nothing) || (comAction /= Nothing) || (mapOut == redIn) 
-}

-- ----------------------------------------------------------------------------
-- Reader and Writer
-- ----------------------------------------------------------------------------

type RawTupleReader k v = B.ByteString -> (k,v)
type RawTupleWriter k v = (k,v) -> B.ByteString

type TextFileReader k v = String -> String -> [(k,v)]
type TextFileWriter k v = [(k,v)] -> String

type ListFileReader k v = [B.ByteString] -> [(k,v)]
type ListFileWriter k v = [(k,v)] -> [B.ByteString]

type BinFileReader  k v = B.ByteString -> [(k,v)]
type BinFileWriter  k v = [(k,v)] -> B.ByteString

data ActionEnvironment = ActionEnvironment {
    ae_TaskData   :: TaskData
  , ae_FileSystem :: Maybe FS.FileSystem
  }

mkActionEnvironment :: TaskData -> Maybe FS.FileSystem -> ActionEnvironment
mkActionEnvironment td fs = ActionEnvironment td fs

data ActionConnector k1 v1 k2 v2 = ActionConnector {
    ac_RawTupleR :: RawTupleReader k1 v1
  , ac_RawTupleW :: RawTupleWriter k2 v2
  , ac_TextR     :: TextFileReader k1 v1
  , ac_TextW     :: TextFileWriter k2 v2
  , ac_ListR     :: ListFileReader k1 v1
  , ac_ListW     :: ListFileWriter k2 v2
  , ac_BinR      :: BinFileReader k1 v1
  , ac_BinW      :: BinFileWriter k2 v2
  }



defaultActionConnector 
  :: (Ord k2,
      Binary k1, Binary v1, Binary k2, Binary v2)
  => ActionConnector k1 v1 k2 v2
defaultActionConnector =
  ActionConnector
    (\b   -> decodeTuple b)
    (\ls  -> encodeTuple ls)
    (\_ _ -> undefined)
    (\_   -> undefined)
    (\bs  -> decodeTupleList bs)
    (\ls  -> encodeTupleList ls)
    (\b   -> decode b)
    (\ls  -> encode ls)


readConnector
  :: (Ord k2,
      Binary k1, Binary v1, Binary k2, Binary v2)
  => ActionConnector k1 v1 k2 v2
  -> ActionEnvironment
  -> [FunctionData] 
  -> IO [(k1,v1)]
readConnector ac ae ls
  = do
    debugM localLogger $ "readConnector: " ++ show ls
    os <- mapM (readInput mbfs) ls
    return $ concat $ catMaybes os
    where
    mbfs = ae_FileSystem ae
    -- readInput :: Maybe FS.FileSystem -> FunctionData -> IO (Maybe [(k1,v1)])
    readInput _         (TupleFunctionData t) = return $ Just $ [(ac_RawTupleR ac) t] 
    readInput (Nothing) (FileFunctionData _)  = return Nothing
    readInput (Just fs) (FileFunctionData f)
      = do        
        debugM localLogger $ "loadInputList: getting content for: " ++ f
        mbc <- FS.getFileContent f fs
        debugM localLogger $ "loadInputList: content is: " ++ show mbc
        if isNothing mbc
          then do
            debugM localLogger $ "loadInputList: no content found"
            return Nothing
          else do
            let c = fromJust mbc
            d <- undefined 
            --  case c of
            --  (FS.TextFile s) -> return $ (ac_TextR ac) f s 
            --  (FS.ListFile l) -> return $ (ac_ListR ac) l -- return $ decodeTupleList l 
            --  (FS.BinFile b)  -> return $ (ac_BinR  ac) b -- return $ decode b
            return $ Just d
            

writeConnector 
  :: (Ord k2,
      Binary k1, Binary v1, Binary k2, Binary v2)
  => ActionConnector k1 v1 k2 v2
  -> ActionEnvironment
  -> [(Int,[(k2,v2)])] 
  -> IO [(Int,[FunctionData])]
writeConnector ac ae ls
  = do
    debugM localLogger $ "writeConnector: "
    os <- mapM (writeOutput mbfs tot) ls
    return $ catMaybes os
    where
    mbfs = ae_FileSystem ae
    td   = ae_TaskData ae
    tot  = td_OutputType $ ae_TaskData ae
    -- writeOutput :: Maybe FS.FileSystem -> TaskOutputType -> (Int,[(k2,v2)]) -> IO (Maybe (Int,[FunctionData]))
    writeOutput _        TOTRawTuple (i,ts) 
      = return $ Just $ (i,bs)
      where
      bs = map (\t -> TupleFunctionData $ (ac_RawTupleW ac) t) ts
    -- TODO exception werfen 
    writeOutput (Nothing) _     _      = return Nothing
    writeOutput (Just fs) t     (i,ts)
      = do
        c <- undefined
        -- c <- case t of
        --  (TOTText) -> return $ FS.TextFile $ (ac_TextW ac) ts
        --  (TOTList) -> return $ FS.ListFile $ (ac_ListW ac) ts
        --  (TOTBin)  -> return $ FS.BinFile  $ (ac_BinW  ac) ts
        -- TODO exception werfen
        --  _         -> undefined
        FS.appendFile fn c fs
        return $ Just (i,[FileFunctionData fn])
        where
        fn = "j" ++ show (td_JobId td) ++ "_t" ++ show (td_TaskId td) ++ "_i" ++ show i
     


type ActionName = String

type ActionInfo = String

data ActionConfiguration a k1 v1 k2 v2 v3 v4
  = ActionConfiguration {
    ac_Name       :: ActionName
  , ac_Info       :: ActionInfo
  , ac_OptDecoder :: OptionsDecoder a
  , ac_Map        :: Maybe (MapConfiguration a k1 v1 k2 v2)
  , ac_Combine    :: Maybe (ReduceConfiguration a k2 v2 v3)
  , ac_Reduce     :: Maybe (ReduceConfiguration a k2 v3 v4)
  }  


data MapConfiguration a k1 v1 k2 v2
  = MapConfiguration {
    mc_Function  :: MapFunction a k1 v1 k2 v2
  , mc_Partition :: MapPartition a k2 v2
  , mc_Connector :: ActionConnector k1 v1 k2 v2
  }


data ReduceConfiguration a k2 v2 v3
  = ReduceConfiguration {
    rc_Merge     :: ReduceMerge a k2 v2
  , rc_Function  :: ReduceFunction a k2 v2 v3
  , rc_Partition :: ReducePartition a k2 v3
  , rc_Connector :: ActionConnector k2 v2 k2 v3
  }

defaultActionConfiguration
  -- :: (Ord k2, Binary a, Binary k1, Binary v1, Binary k2, Binary v2, Binary v3)
  :: (Binary a) => ActionName
  -> ActionConfiguration a k1 v1 k2 v2 v3 v4
defaultActionConfiguration name
  = ActionConfiguration
      name
      ""
      defaultOptionsDecoder
      Nothing
      Nothing
      Nothing

defaultMapConfiguration
  :: (Ord k2, Binary a, Binary k1, Binary v1, Binary k2, Binary v2)
  => MapFunction a k1 v1 k2 v2
  -> MapConfiguration a k1 v1 k2 v2
defaultMapConfiguration fct
  = MapConfiguration
      fct
      defaultPartition
      defaultActionConnector


defaultReduceConfiguration
  :: (Ord k2, Binary a, Binary k2, Binary v2, Binary v3)
  => ReduceFunction a k2 v2 v3
  -> ReduceConfiguration a k2 v2 v3
defaultReduceConfiguration fct
  = ReduceConfiguration
      defaultMerge
      fct
      defaultPartition
      defaultActionConnector


defaultPartition
  :: (Binary k2, Binary v2)
  => MapPartition a k2 v2
defaultPartition _ n ls
  = do
    -- calculate partition-Values
    let markedList = map (\t@(k,_) -> (hash k,t)) ls
    -- merge them
    let resultList = AMap.toList $ AMap.fromTupleList markedList
    return resultList
    where
    -- calculate a hash-value, because we only have the Binary-Instance, we
    -- can only use the Bytestring of the Value
    hash k = (fromIntegral $ Hash.hashString (show $ encode k)) `mod` n


defaultMerge
  :: (Ord k2, Binary k2, Binary v2)
  => ReduceMerge a k2 v2
defaultMerge _ ls
  = return $ AMap.toList $ AMap.fromTupleList ls



data ActionData
  = ActionData {
    ad_Name    :: ActionName
  , ad_Info    :: ActionInfo
  , ad_Map     :: Maybe BinaryMapAction
  , ad_Combine :: Maybe BinaryReduceAction
  , ad_Reduce  :: Maybe BinaryReduceAction
  }

instance KMap.Key ActionData where
  getKey = ad_Name

instance Show ActionData where
  show (ActionData n i _ _ _) = "{ActionData name:\"" ++ n ++ "\" info:\"" ++ i ++ "\"}"


type ActionMap = KMap.KeyMap ActionData


type OptionsDecoder a = B.ByteString -> a

defaultOptionsDecoder :: (Binary a) => OptionsDecoder a
defaultOptionsDecoder = decode

getActionForTaskType :: TaskType -> ActionData -> Maybe BinaryReduceAction
getActionForTaskType TTMap     ad = ad_Map ad
getActionForTaskType TTCombine ad = ad_Combine ad
getActionForTaskType TTReduce  ad = ad_Reduce ad
getActionForTaskType _         _  = Nothing

  

readActionConfiguration
  :: ( Ord k2, Binary a
     , Binary k1, Binary v1
     , Binary k2, Binary v2
     , Binary v3, Binary v4)
  => ActionConfiguration a k1 v1 k2 v2 v3 v4 
  -> ActionData
readActionConfiguration
  (ActionConfiguration n i d mc cc rc)
  = ActionData n i mf cf rf
  where
  mf = maybe Nothing (\(MapConfiguration f p c) -> Just $ performMapAction d f p c) mc
  cf = maybe Nothing (\(ReduceConfiguration m f p c) -> Just $ performReduceAction d m f p c) cc
  rf = maybe Nothing (\(ReduceConfiguration m f p c) -> Just $ performReduceAction d m f p c) rc


-- ----------------------------------------------------------------------------
-- MapAction
-- ----------------------------------------------------------------------------


-- | general MapAction
type MapAction a k1 v1 k2 v2 = a -> Int -> [(k1,v1)] -> IO [(Int, [(k2,v2)])]

-- | MapAction on ByteStrings
type BinaryMapAction = ActionEnvironment -> B.ByteString -> Int -> [FunctionData] -> IO [(Int, [FunctionData])] 

type MapFunction a k1 v1 k2 v2 = a -> k1 -> v1 -> IO [(k2, v2)]

type MapPartition a k2 v2 = a -> Int -> [(k2,v2)] -> IO [(Int, [(k2,v2)])]
    

performMapAction
  :: (Ord k2,
      Binary a, Binary k1, Binary v1, Binary k2, Binary v2)
  => OptionsDecoder a
  -> MapFunction a k1 v1 k2 v2
  -> MapPartition a k2 v2
  -> ActionConnector k1 v1 k2 v2
  -> ActionEnvironment
  -> B.ByteString
  -> Int
  -> [FunctionData]
  -> IO [(Int, [FunctionData])]
performMapAction optDec fct part conn env opts n ls
  = do
    -- decode the options
    let a = optDec opts
    
    infoM localLogger "performMapAction"
    
    infoM localLogger "reading inputList"
    inputList <- readConnector conn env ls
    
    infoM localLogger "doing map"
    tupleList <- mapM (\(k1, v1) -> fct a k1 v1) inputList
    
    infoM localLogger "doing partition"
    partedList <- part a n $ concat tupleList
    
    infoM localLogger "writing outputlist"
    outputList <- writeConnector conn env partedList
    return outputList





-- ----------------------------------------------------------------------------
-- Combine- / ReduceTask
-- ----------------------------------------------------------------------------

-- | general MapAction
type ReduceAction a k2 v2 v3 = a -> Int -> [(k2,v2)] -> IO [(Int, [(k2,v3)])]

-- | MapAction on ByteStrings
type BinaryReduceAction = ActionEnvironment -> B.ByteString -> Int -> [FunctionData] -> IO [(Int, [FunctionData])] 

type ReduceMerge a k2 v2 = a -> [(k2,v2)] -> IO [(k2,[v2])]

type ReduceFunction a k2 v2 v3 = a -> k2 -> [v2] -> IO (Maybe v3)

type ReducePartition a k2 v3 = a -> Int -> [(k2,v3)] -> IO [(Int, [(k2,v3)])]
    

performReduceAction
  :: (Ord k2,
      Binary a, Binary k2, Binary v2, Binary v3)
  => OptionsDecoder a
  -> ReduceMerge a k2 v2
  -> ReduceFunction a k2 v2 v3
  -> ReducePartition a k2 v3
  -> ActionConnector k2 v2 k2 v3
  -> ActionEnvironment
  -> B.ByteString
  -> Int
  -> [FunctionData]
  -> IO [(Int, [FunctionData])]
performReduceAction optDec merge fct part conn env opts n ls
  = do
    -- decode the options
    let a = optDec opts
  
    infoM localLogger "performReduceAction"
    
    infoM localLogger "reading inputList"
    inputList <- readConnector conn env ls
    
    infoM localLogger "doing merge"
    mergedList <- merge a inputList
    
    infoM localLogger "doing reduce"
    maybesList <- mapM (\(k2,v2s) -> performReduceFunction a k2 v2s) mergedList
    
    infoM localLogger "doing partition" 
    partedList <- part a n $ catMaybes maybesList
    
    infoM localLogger "writing outputlist"
    outputList <- writeConnector conn env partedList
    
    return outputList
    where
      performReduceFunction a k2 v2s
        = do
          mbV3 <- fct a k2 v2s
          case mbV3 of
            (Nothing) -> return Nothing
            (Just v3) -> return $ Just (k2,v3)
