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
, FunctionData(..)

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

-- * JobTest
, testJobInfo

-- * TaskAction
, ActionEnvironment(..)
, mkActionEnvironment
, ActionConnector(..)
, defaultActionConnector

-- * MapAction
, MapAction
, BinaryMapAction 
, MapFunction
, MapPartition
, MapActionData(..)
, MapActionMap
, mkMapAction

-- * Combine-Reduce-Action
, ReduceAction
, BinaryReduceAction 
, ReduceFunction
, ReducePartition
, ReduceActionData(..)
, ReduceActionMap
, mkReduceAction
)
where

import           Control.Concurrent
import           Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Typeable
import           Data.Time

import           System.Log.Logger

import           Text.XML.HXT.Arrow

import           Holumbus.Common.Utils
import           Holumbus.MapReduce.TypeCheck
import qualified Holumbus.Data.AccuMap as AMap
import qualified Holumbus.FileSystem.FileSystem as FS


localLogger :: String
localLogger = "Holumbus.MapReduce.Types"

-- ----------------------------------------------------------------------------
-- general datatypes
-- ----------------------------------------------------------------------------


type FunctionName = String

type FunctionInfo = String

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
  , td_Input      :: ! [FunctionData]
  , td_Output     :: ! [(Int,[FunctionData])]
  , td_OutputType :: ! TaskOutputType
  , td_Action     :: ! FunctionName
  } deriving (Show, Eq, Ord)

instance Binary TaskData where
  put (TaskData jid tid tt ts i o ot a)
    = put jid >> put tid >> put tt >> put ts >> put i >> put o >> put ot >> put a
  get
    = do
      jid <- get
      tid <- get
      tt  <- get
      ts  <- get
      i   <- get
      o   <- get
      ot  <- get
      a   <- get
      return (TaskData jid tid tt ts i o ot a)




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
  , ji_MapAction         :: ! (Maybe FunctionName)
  , ji_CombineAction     :: ! (Maybe FunctionName)
  , ji_ReduceAction      :: ! (Maybe FunctionName)
  , ji_MapOutputType     :: ! (Maybe TaskOutputType)
  , ji_CombineOutputType :: ! (Maybe TaskOutputType)
  , ji_ReduceOutputType  :: ! (Maybe TaskOutputType)
  , ji_Input             :: ! [FunctionData]
  } deriving (Show, Eq)

instance Binary JobInfo where
  put (JobInfo d ma ca ra mo co ro i)
    = put d >> put ma >> put ca >> put ra >> put mo >> put co >> put ro >> put i
  get
    = do
      d <- get
      ma <- get
      ca <- get
      ra <- get
      mo <- get
      co <- get
      ro <- get      
      i <- get
      return (JobInfo d ma ca ra mo co ro i)


instance XmlPickler JobInfo where
  xpickle = xpJobInfo
  
xpJobInfo :: PU JobInfo
xpJobInfo = 
    xpElem "jobInfo" $
    xpWrap
      (\(n, (ma,mo), (ca, co), (ra,ro), i) -> JobInfo n ma ca ra mo co ro i, 
       \(JobInfo n ma ca ra mo co ro i) -> (n, (ma,mo), (ca, co), (ra,ro), i)) xpJob
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
      xpTaskAction = xpPair (xpFunction) (xpickle)


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
  :: (Ord k2, Show k1, Show v1, Show k2, Show v2,
      Binary k1, Binary v1, Binary k2, Binary v2)
  => ActionConnector k1 v1 k2 v2
defaultActionConnector =
  ActionConnector
    (\b   -> decodeTuple b)
    (\ls  -> encodeTuple ls)
    (\k v -> decode $ encode [(k,v)])
    (\ls  -> show ls)
    (\bs  -> decodeTupleList bs)
    (\ls  -> encodeTupleList ls)
    (\b   -> decode b)
    (\ls  -> encode ls)
  
readConnector
  :: (Ord k2, Show k1, Show v1, Show k2, Show v2,
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
            d <- case c of
              (FS.TextFile s) -> return $ (ac_TextR ac) f s 
              (FS.ListFile l) -> return $ (ac_ListR ac) l -- return $ decodeTupleList l 
              (FS.BinFile b)  -> return $ (ac_BinR  ac) b -- return $ decode b
            return $ Just d
            

writeConnector 
  :: (Ord k2, Show k1, Show v1, Show k2, Show v2,
      Binary k1, Binary v1, Binary k2, Binary v2)
  => ActionConnector k1 v1 k2 v2
  -> ActionEnvironment
  -> [(Int,[(k2,v2)])] 
  -> IO [(Int,[FunctionData])]
writeConnector ac ae ls
  = do
    debugM localLogger $ "writeConnector: " ++ show ls
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
        c <- case t of
          (TOTText) -> return $ FS.TextFile $ (ac_TextW ac) ts
          (TOTList) -> return $ FS.ListFile $ (ac_ListW ac) ts
          (TOTBin)  -> return $ FS.BinFile  $ (ac_BinW  ac) ts
          -- TODO exception werfen
          _         -> undefined
        FS.appendFile fn c fs
        return $ Just (i,[FileFunctionData fn])
        where
        fn = "j" ++ show (td_JobId td) ++ "_t" ++ show (td_TaskId td) ++ "_i" ++ show i
     

-- ----------------------------------------------------------------------------
-- MapAction
-- ----------------------------------------------------------------------------

-- | general MapAction
type MapAction k1 v1 k2 v2 = Int -> [(k1,v1)] -> IO [(Int, [(k2,v2)])]

-- | MapAction on ByteStrings
type BinaryMapAction = ActionEnvironment -> Int -> [FunctionData] -> IO [(Int, [FunctionData])] 

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
  -> ActionConnector k1 v1 k2 v2
  -> MapActionData
mkMapAction name info fct part ae
  = MapActionData name info action inputType outputType
    where
      action = performMapAction ae fct part
      inputType = makeTuple $ take 2 $ getFunctionParameters $ typeOf fct
      outputType = last $ getTupleParameters $ getListParameter $ getIOParameter $ last $ getFunctionParameters $ typeOf part



performMapAction
  :: (Ord k2, Show k1, Show v1, Show k2, Show v2, 
      Binary k1, Binary v1, Binary k2, Binary v2)
  => ActionConnector k1 v1 k2 v2
  -> MapFunction k1 v1 k2 v2
  -> MapPartition k2 v2
  -> ActionEnvironment
  -> Int
  -> [FunctionData]
  -> IO [(Int, [FunctionData])]
performMapAction ac fct part ae n ls
  = do
    infoM localLogger "performMapAction"
    debugM localLogger $ "ls: " ++ show ls
    
    infoM localLogger "reading inputList"
    inputList <- readConnector ac ae ls
    
    infoM localLogger "doing map"
    tupleList <- mapM (\(k1, v1) -> fct k1 v1) inputList
    
    infoM localLogger "doing partition"
    partedList <- part n $ concat tupleList
    
    infoM localLogger "writing outputlist"
    outputList <- writeConnector ac ae partedList
    return outputList
{-
-- | a wrapper for invoking genaral MapFunctions from ByteStrings
encodeMapAction 
  :: (Ord k2, Show k1, Show v1, Show k2, Show v2,
      Binary k1, Binary v1, Binary k2, Binary v2)
  -> MapAction k1 v1 k2 v2
  -> ActionEnvironment
  -> Int
  -> [FunctionData] -> IO [(Int, [FunctionData])] 
encodeMapAction ac f env n inls
  = do
-}  
    -- let inputList = decodeTupleList b
    -- return $ map (\(i, l) -> (i, encodeTupleList l)) outputList

-- ----------------------------------------------------------------------------
-- Combine- / ReduceTask
-- ----------------------------------------------------------------------------

-- | general MapAction
type ReduceAction k2 v2 v3 = Int -> [(k2,v2)] -> IO [(Int, [(k2,v3)])]

-- | MapAction on ByteStrings
type BinaryReduceAction = ActionEnvironment -> Int -> [FunctionData] -> IO [(Int, [FunctionData])] 

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
  -> ActionConnector k2 v2 k2 v3
  -> ReduceActionData
mkReduceAction name info merge fct part ae
  = ReduceActionData name info action inputType outputType
    where
      action = performReduceAction ae merge fct part
      inputType = head $ getFunctionParameters $ typeOf merge
      outputType = last $ getTupleParameters $ getListParameter $ getIOParameter $ last $ getFunctionParameters $ typeOf part


performReduceAction
  :: (Ord k2, Show k2, Show v2, Show v3, 
      Binary k2, Binary v2, Binary v3)
  => ActionConnector k2 v2 k2 v3
  -> ReduceMerge k2 v2
  -> ReduceFunction k2 v2 v3
  -> ReducePartition k2 v3
  -> ActionEnvironment
  -> Int
  -> [FunctionData]
  -> IO [(Int, [FunctionData])]
performReduceAction ac merge fct part ae n ls
  = do
    infoM localLogger "performReduceAction"
    infoM localLogger $ show ls
    
    infoM localLogger "reading inputList"
    inputList <- readConnector ac ae ls
    
    infoM localLogger "doing merge"
    mergedList <- merge inputList
    
    infoM localLogger "doing reduce"
    maybesList <- mapM (\(k2,v2s) -> performReduceFunction k2 v2s) mergedList
    
    infoM localLogger "doing partition" 
    partedList <- part n $ catMaybes maybesList
    
    infoM localLogger "writing outputlist"
    outputList <- writeConnector ac ae partedList
    
    
    return outputList
    where
      performReduceFunction k2 v2s
        = do
          mbV3 <- fct k2 v2s
          case mbV3 of
            (Nothing) -> return Nothing
            (Just v3) -> return $ Just (k2,v3)

{-
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
-}