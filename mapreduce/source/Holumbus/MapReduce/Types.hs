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
  FunctionName
, FunctionInfo
, FunctionData

, encodeTuple
, decodeTuple
, decodeTupleList
, encodeTupleList

-- * TaskData
, TaskId
, TaskType(..)
, TaskState(..)
, getNextTaskState
, TaskAction
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

-- * JobTest
, testJobInfo

-- * MapAction
, MapAction
, BinaryMapAction 
, MapFunction
, MapPartition
, MapActionData(..)
, MapActionMap
, mkMapAction

-- * Combine-/Reduce-Action
, ReduceAction
, BinaryReduceAction 
, ReduceFunction
, ReducePartition
, ReduceActionData(..)
, ReduceActionMap
, mkReduceAction

{-
-- * Map-Function
, MapFunction
, BinaryMapFunction

, MapFunctionMap 
, emptyMapFunctionMap
, addMapFunctionToMap
, dispatchMapFunction
, listMapFunctions

-- * Reduce/Combine-Function
, ReduceFunction
, BinaryReduceFunction

, ReduceFunctionMap
, emptyReduceFunctionMap
, addReduceFunctionToMap
, dispatchReduceFunction
, listReduceFunctions

-- * Merge-Function
, MergeFunction
, BinaryMergeFunction

, MergeFunctionMap
, emptyMergeFunctionMap
, addMergeFunctionToMap
, dispatchMergeFunction
, listMergeFunctions

-- * Partition-Function
, PartitionFunction
, BinaryPartitionFunction

, PartitionFunctionMap
, emptyPartitionFunctionMap
, addPartitionFunctionToMap
, dispatchPartitionFunction
, listPartitionFunctions
-}
)
where

import           Control.Concurrent
import           Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Typeable
import           Data.Time

import           Text.XML.HXT.Arrow

import           Holumbus.Common.Utils
import           Holumbus.MapReduce.TypeCheck
import qualified Holumbus.MapReduce.AccuMap as AMap




-- ----------------------------------------------------------------------------
-- general datatypes
-- ----------------------------------------------------------------------------


type FunctionName = String

type FunctionInfo = String

type FunctionData = B.ByteString


-- ----------------------------------------------------------------------------
-- general encoding / decoding
-- ----------------------------------------------------------------------------

encodeTuple :: (Binary k, Binary v) => (k, v) -> B.ByteString
encodeTuple t = encode t


decodeTuple :: (Binary k, Binary v) => B.ByteString -> (k, v)
decodeTuple b = decode b


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


type TaskAction = String
  

-- | the TaskData, contains all information to do the task
data TaskData = TaskData {
    td_JobId     :: ! JobId
  , td_TaskId    :: ! TaskId
  , td_Type      :: TaskType
  , td_State     :: TaskState
  , td_Input     :: [FunctionData]
  , td_Output    :: [(Int,[FunctionData])]
  , td_Action    :: TaskAction
  } deriving (Show, Eq, Ord)

instance Binary TaskData where
  put (TaskData jid tid tt ts i o a)
    = put jid >> put tid >> put tt >> put ts >> put i >> put o >> put a
  get
    = do
      jid <- get
      tid <- get
      tt <- get
      ts <- get
      i <- get
      o <- get
      a <- get
      return (TaskData jid tid tt ts i o a)




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
    ji_Description      :: ! String
  , ji_MapAction        :: ! (Maybe TaskAction)
  , ji_CombineAction    :: ! (Maybe TaskAction)
  , ji_ReduceAction     :: ! (Maybe TaskAction)
  , ji_Input            :: ! [(String, String)]
  } deriving (Show)

instance Binary JobInfo where
  put (JobInfo d m c r i)
    = put d >> put m >> put c >> put r >> put i
  get
    = do
      d <- get
      m <- get
      c <- get
      r <- get
      i <- get
      return (JobInfo d m c r i)


instance XmlPickler JobInfo where
  xpickle = xpJobInfo
  
xpJobInfo :: PU JobInfo
xpJobInfo = 
    xpElem "jobInfo" $
    xpWrap (\(n, m, c, r, i) -> JobInfo n m c r i, \(JobInfo n m c r i) -> (n, m, c, r, i)) xpJob
    where
    xpJob = 
      xp5Tuple
        (xpAttr "name" xpText0)
        (xpMapAction)
        (xpCombineAction)
        (xpReduceAction)
        (xpFunctionDataList)
      where 
      xpMapAction     = xpOption $ xpElem "map" $ xpTaskAction
      xpCombineAction = xpOption $ xpElem "combine" $ xpTaskAction
      xpReduceAction  = xpOption $ xpElem "reduce" $ xpTaskAction
      xpTaskAction    = xpAttr "action" xpText
      xpFunctionDataList
        = xpWrap (filterEmptyList,setEmptyList) $ xpOption $
          xpElem "inputList" $ xpList xpFunctionData
      xpFunctionData
        = xpElem "input" $
          xpPair xpKey xpValue
          where
            xpKey = xpElem "key" xpText0
            xpValue = xpElem "value" xpText0


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


-- ----------------------------------------------------------------------------
-- TestJobInfo
-- ----------------------------------------------------------------------------


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
  mapName   = ji_MapAction ji
  comName   = ji_CombineAction ji
  redName   = ji_ReduceAction ji
  mapOut    = mad_OutputType $ fromJust $ Map.lookup (fromJust mapName) mm
  comOut    = rad_OutputType $ fromJust $ Map.lookup (fromJust comName) rm
  comIn     = rad_InputType $ fromJust $ Map.lookup (fromJust comName) rm
  redIn     = rad_InputType $ fromJust $ Map.lookup (fromJust redName) rm
  testNames = (mapName /= Nothing) || (comName /= Nothing) || (redName /= Nothing)
  testMap   = (mapName == Nothing) || (isJust $ Map.lookup (fromJust mapName) mm)
  testCom   = (comName == Nothing) || (isJust $ Map.lookup (fromJust comName) rm)
  testRed   = (redName == Nothing) || (isJust $ Map.lookup (fromJust redName) rm)
  testIOMC  = (mapName == Nothing) || (comName == Nothing) || (mapOut == comIn)
  testIOCR  = (comName == Nothing) || (redName == Nothing) || (comOut == redIn)
  testIOMR  = (mapName == Nothing) || (redName == Nothing) || (comName /= Nothing) || (mapOut == redIn) 


-- ----------------------------------------------------------------------------
-- MapAction
-- ----------------------------------------------------------------------------

-- | general MapAction
type MapAction k1 v1 k2 v2 = Int -> [(k1,v1)] -> IO [(Int, [(k2,v2)])]

-- | MapAction on ByteStrings
type BinaryMapAction = Int -> [B.ByteString] -> IO [(Int, [B.ByteString])] 

type MapFunction k1 v1 k2 v2 = (k1 -> v1 -> IO [(k2, v2)])

type MapPartition k2 v2 = Int -> [(k2,v2)] -> IO [(Int, [(k2,v2)])]
    
data MapActionData = MapActionData {
    mad_Name       :: FunctionName
  , mad_Info       :: String
  , mad_Action     :: BinaryMapAction
  , mad_InputType  :: TypeRep
  , mad_OutputType :: TypeRep
  }
  
instance Show MapActionData where
  show (MapActionData n i _ input output) =
    "{MapAction: Name: " ++ n ++ ", Info: " ++ i ++ 
    ", Input: " ++ (show input) ++ ", Output: " ++ (show output) ++ "}"

-- | map for storing the MapFunctions    
type MapActionMap = Map.Map FunctionName MapActionData

mkMapAction
  :: (Ord k2, Show k1, Show v1, Show k2, Show v2, 
      Binary k1, Binary v1, Binary k2, Binary v2,
      Typeable k1, Typeable k2, Typeable v1, Typeable v2)
  => FunctionName 
  -> FunctionInfo
  -> MapFunction k1 v1 k2 v2
  -> MapPartition k2 v2
  -> MapActionData
mkMapAction name info fct part
  = MapActionData name info action inputType outputType
    where
      action = encodeMapAction $ performMapAction fct part
      inputType = makeTuple $ take 2 $ getFunctionParameters $ typeOf fct
      outputType = last $ getTupleParameters $ getListParameter $ getIOParameter $ last $ getFunctionParameters $ typeOf part



performMapAction
  :: (Ord k2, Show k1, Show v1, Show k2, Show v2, 
      Binary k1, Binary v1, Binary k2, Binary v2)
  => MapFunction k1 v1 k2 v2
  -> MapPartition k2 v2
  -> Int
  -> [(k1,v1)]
  -> IO [(Int, [(k2,v2)])]
performMapAction fct part n ls
  = do
    putStrLn "performMapAction"
    putStrLn $ "ls: " ++ show ls
    tupleList <- mapM (\(k1, v1) -> fct k1 v1) ls
    partedList <- part n $ concat tupleList
    return partedList

-- | a wrapper for invoking genaral MapFunctions from ByteStrings
encodeMapAction 
  :: (Ord k2, Show k1, Show v1, Show k2, Show v2,
      Binary k1, Binary v1, Binary k2, Binary v2)
  => MapAction k1 v1 k2 v2 
  -> Int
  -> [B.ByteString] -> IO [(Int, [B.ByteString])] 
encodeMapAction f n b
  = do
    let inputList = decodeTupleList b
    outputList <- f n inputList
    return $ map (\(i, l) -> (i, encodeTupleList l)) outputList


-- ----------------------------------------------------------------------------
-- Combine- / ReduceTask
-- ----------------------------------------------------------------------------

-- | general MapAction
type ReduceAction k2 v2 v3 = Int -> [(k2,v2)] -> IO [(Int, [(k2,v3)])]

-- | MapAction on ByteStrings
type BinaryReduceAction = Int -> [B.ByteString] -> IO [(Int, [B.ByteString])] 

type ReduceMerge k2 v2 = [(k2,v2)] -> IO [(k2,[v2])]

type ReduceFunction k2 v2 v3 = (k2 -> [v2] -> IO (Maybe v3))

type ReducePartition k2 v3 = Int -> [(k2,v3)] -> IO [(Int, [(k2,v3)])]
    
data ReduceActionData = ReduceActionData {
    rad_Name       :: FunctionName
  , rad_Info       :: String
  , rad_Action     :: BinaryReduceAction
  , rad_InputType  :: TypeRep
  , rad_OutputType :: TypeRep
  }
  
instance Show ReduceActionData where
  show (ReduceActionData n i _ input output) =
    "{ReduceAction: Name: " ++ n ++ ", Info: " ++ i ++ 
    ", Input: " ++ (show input) ++ ", Output: " ++ (show output) ++ "}"


-- | Map for storing the ReduceFunctions    
type ReduceActionMap = Map.Map FunctionName ReduceActionData


mkReduceAction
  :: (Ord k2, Show k2, Show v2, Show v3, 
      Binary k2, Binary v2, Binary v3,
      Typeable k2, Typeable v2, Typeable v3)
  => FunctionName 
  -> FunctionInfo
  -> ReduceMerge k2 v2
  -> ReduceFunction k2 v2 v3
  -> ReducePartition k2 v3
  -> ReduceActionData
mkReduceAction name info merge fct part
  = ReduceActionData name info action inputType outputType
    where
      action = encodeReduceAction $ performReduceAction merge fct part
      inputType = head $ getFunctionParameters $ typeOf merge
      outputType = last $ getTupleParameters $ getListParameter $ getIOParameter $ last $ getFunctionParameters $ typeOf part


performReduceAction
  :: (Ord k2, Show k2, Show v2, Show v3, 
      Binary k2, Binary v2, Binary v3)
  => ReduceMerge k2 v2
  -> ReduceFunction k2 v2 v3
  -> ReducePartition k2 v3
  -> Int
  -> [(k2,v2)]
  -> IO [(Int, [(k2,v3)])]
performReduceAction merge fct part n ls
  = do
    putStrLn "performReduceAction"
    putStrLn $ show ls
    mergedList <- merge ls
    maybesList <- mapM (\(k2,v2s) -> performReduceFunction k2 v2s) mergedList 
    partedList <- part n $ catMaybes maybesList
    return partedList
    where
      performReduceFunction k2 v2s
        = do
          mbV3 <- fct k2 v2s
          case mbV3 of
            (Nothing) -> return Nothing
            (Just v3) -> return $ Just (k2,v3)


-- | a wrapper for invoking genaral ReduceFunctions from ByteStrings
encodeReduceAction 
  :: (Ord k2, Show k2, Show v2, Show v3,
      Binary k2, Binary v2, Binary v3)
  => ReduceAction k2 v2 v3
  -> Int
  -> [B.ByteString] -> IO [(Int, [B.ByteString])] 
encodeReduceAction f n b
  = do
    let inputList = decodeTupleList b
    outputList <- f n inputList
    return $ map (\(i, l) -> (i, encodeTupleList l)) outputList

{-

data ReduceFunctionMap = ReduceFunctionMap (Map.Map FunctionName ReduceFunctionData)

instance Show ReduceFunctionMap where
  show _ = "ReduceFunctionMap"


-- | a wrapper for invoking genaral MapFunctions from ByteString from 
--   input and output  
encodeReduceFunction
  :: (Binary k1, Binary v1, Binary v2)
  => ReduceFunction k1 v1 v2 
  -> B.ByteString -> IO (Maybe B.ByteString)
encodeReduceFunction f b
  = do
    --TODO catch exception...
    let (k, vs) = decodeTuple b   
    r <- f k vs
    case r of
      (Nothing) -> return Nothing
      (Just v)  -> return $ Just $ encodeTuple (k, v)


emptyReduceFunctionMap :: ReduceFunctionMap
emptyReduceFunctionMap = ReduceFunctionMap Map.empty


addReduceFunctionToMap
  :: (Typeable k1, Typeable v1, Typeable v2, 
     Binary k1, Binary v1, Binary v2)
  => ReduceFunction k1 v1 v2
  -> FunctionName
  -> FunctionDescription
  -> ReduceFunctionMap
  -> ReduceFunctionMap
addReduceFunctionToMap f n d (ReduceFunctionMap m) 
  = ReduceFunctionMap $ Map.insert n (n,d,t,f') m
  where
    f' = encodeReduceFunction f
    t  = typeOf f


dispatchReduceFunction :: ReduceFunctionMap -> Maybe FunctionName -> Maybe BinaryReduceFunction
dispatchReduceFunction _ Nothing = Nothing
dispatchReduceFunction (ReduceFunctionMap mfm) (Just fn)
  = check $ Map.lookup fn mfm
    where
      check (Nothing) = Nothing
      check (Just (_,_,_,f)) = Just f
      

listReduceFunctions :: ReduceFunctionMap -> [(FunctionName, FunctionDescription, TypeRep)]
listReduceFunctions (ReduceFunctionMap m) = map (\(n,d,t,_)->(n,d,t)) (Map.elems m)
-}

-- ----------------------------------------------------------------------------
-- Merge-Function
-- ----------------------------------------------------------------------------

{-
type MergeFunction k1 v1 v2 = [(k1,v1)] -> IO [(k1,v2)]

type BinaryMergeFunction = [B.ByteString] -> IO [B.ByteString]

type MergeFunctionData = (FunctionName, FunctionDescription, TypeRep, BinaryMergeFunction)

data MergeFunctionMap = MergeFunctionMap (Map.Map FunctionName MergeFunctionData)

instance Show MergeFunctionMap where
  show _ = "MergeFunctionMap"


encodeMergeFunction
  :: (Binary k1, Binary v1, Binary v2)
  => MergeFunction k1 v1 v2
  -> [B.ByteString] -> IO [B.ByteString]
encodeMergeFunction f ls
  = do  
    rs <- f (decodeTupleList ls)
    return $ encodeTupleList rs


emptyMergeFunctionMap :: MergeFunctionMap
emptyMergeFunctionMap = MergeFunctionMap Map.empty


addMergeFunctionToMap
  :: (Typeable k1, Typeable v1, Typeable v2,
     Binary k1, Binary v1, Binary v2)
  => MergeFunction k1 v1 v2
  -> FunctionName
  -> FunctionDescription
  -> MergeFunctionMap
  -> MergeFunctionMap
addMergeFunctionToMap f n d (MergeFunctionMap m) 
  = MergeFunctionMap $ Map.insert n (n,d,t,f') m
  where
    f' = encodeMergeFunction f
    t  = typeOf f


dispatchMergeFunction :: MergeFunctionMap -> Maybe FunctionName -> Maybe BinaryMergeFunction
dispatchMergeFunction _ Nothing = Nothing
dispatchMergeFunction (MergeFunctionMap mfm) (Just fn)
  = check $ Map.lookup fn mfm
    where
      check (Nothing) = Nothing
      check (Just (_,_,_,f)) = Just f
      

listMergeFunctions :: MergeFunctionMap -> [(FunctionName, FunctionDescription, TypeRep)]
listMergeFunctions (MergeFunctionMap m) = map (\(n,d,t,_)->(n,d,t)) (Map.elems m)



-- ----------------------------------------------------------------------------
-- Partition
-- ----------------------------------------------------------------------------

type PartitionFunction k1 v1 = Int -> [(k1,v1)] -> IO [(Int, [(k1,v1)])]

type BinaryPartitionFunction = Int -> [B.ByteString] -> IO [B.ByteString]

type PartitionFunctionData = (FunctionName, FunctionDescription, TypeRep, BinaryPartitionFunction)

data PartitionFunctionMap = PartitionFunctionMap (Map.Map FunctionName PartitionFunctionData)

instance Show PartitionFunctionMap where
  show _ = "PartitionFunctionMap"


encodePartitionFunction
  :: (Binary k1, Binary v1)
  => PartitionFunction k1 v1 
  -> Int -> [B.ByteString] -> IO [B.ByteString]
encodePartitionFunction f n ls
  = do  
    rs <- f n (decodeTupleList ls)
    --TODO hier Probleme mit encoding... vermutlich
    return $ encodeTupleList rs


emptyPartitionFunctionMap :: PartitionFunctionMap
emptyPartitionFunctionMap = PartitionFunctionMap Map.empty


addPartitionFunctionToMap
  :: (Typeable k1, Typeable v1,
     Binary k1, Binary v1)
  => PartitionFunction k1 v1
  -> FunctionName
  -> FunctionDescription
  -> PartitionFunctionMap
  -> PartitionFunctionMap
addPartitionFunctionToMap f n d (PartitionFunctionMap m) 
  = PartitionFunctionMap $ Map.insert n (n,d,t,f') m
  where
    f' = encodePartitionFunction f
    t  = typeOf f


dispatchPartitionFunction :: PartitionFunctionMap -> Maybe FunctionName -> Maybe BinaryPartitionFunction
dispatchPartitionFunction _ Nothing = Nothing
dispatchPartitionFunction (PartitionFunctionMap mfm) (Just fn)
  = check $ Map.lookup fn mfm
    where
      check (Nothing) = Nothing
      check (Just (_,_,_,f)) = Just f
      

listPartitionFunctions :: PartitionFunctionMap -> [(FunctionName, FunctionDescription, TypeRep)]
listPartitionFunctions (PartitionFunctionMap m) = map (\(n,d,t,_)->(n,d,t)) (Map.elems m)
-}