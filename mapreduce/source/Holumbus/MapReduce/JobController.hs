-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.MapReduce.JobController
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts #-}
module Holumbus.MapReduce.JobController
(
  JobId
, TaskId

, JobState(..)

, TaskType(..)
, TaskState(..)

, TaskData(..)

, JobInfo(..)
, JobData(..)
, JobResult(..)

, TaskSendResult(..)
, TaskSendFunction

, JobControlData(..)
, newJobControlData

, createNewJob

--, newJobData
--, newTaskData

, getJobIds
, getTaskIds

, doControlling
)
where

import Control.Concurrent
import Control.Monad
import Data.Binary
import Data.Maybe
import Data.Time
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO.Unsafe

import Holumbus.MapReduce.Types
import Holumbus.FileSystem.Storage as S
import qualified Holumbus.MapReduce.MultiMap as MMap


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
        

-- | the TaskData, contains all information to do the task
data TaskData = TaskData {
    td_JobId     :: ! JobId
  , td_TaskId    :: ! TaskId
  , td_Type      :: TaskType
  , td_State     :: TaskState
  , td_Input     :: Maybe FunctionData
  , td_Output    :: Maybe FunctionData
  , td_Action    :: FunctionName
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
data JobState = JSPlanned | JSIdle | JSMap | JSCombine | JSReduce | JSFinished | JSError
  deriving(Show, Eq, Ord, Enum)

instance Binary JobState where
  put (JSPlanned)    = putWord8 1
  put (JSIdle)       = putWord8 2
  put (JSMap)        = putWord8 3
  put (JSCombine)    = putWord8 4
  put (JSReduce)     = putWord8 5
  put (JSFinished)   = putWord8 6
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
        6 -> return (JSFinished)
        _ -> return (JSError)




type OutputMap = MMap.MultiMap JobState FunctionData


-- | defines a job, this is all data the user has to give to run a job
data JobInfo = JobInfo {
    ji_Descrition      :: ! String
  -- , ji_PartitionAction :: ! (Maybe TaskAction)
  , ji_MapAction       :: ! (Maybe FunctionName)
  , ji_CombineAction   :: ! (Maybe FunctionName)
  , ji_ReduceAction    :: ! (Maybe FunctionName)
  , ji_Input           :: ! [FunctionData]
  , ji_Ouput           :: ! [FunctionData]
  } deriving (Show)

instance Binary JobInfo where
  put (JobInfo d m c r i o)
    = put d >> put m >> put c >> put r >> put i >> put o
  get
    = do
      d <- get
      m <- get
      c <- get
      r <- get
      i <- get
      o <- get
      return (JobInfo d m c r i o)


-- | the job data, include the user-input and some additional control-data
data JobData = JobData {
    jd_JobId       :: JobId
  , jd_State       :: JobState
  , jd_OutputMap   :: OutputMap
  , jd_Info        :: JobInfo
  , jd_startTime   :: UTCTime
  , jd_endTime     :: UTCTime
  } deriving (Show)


-- | the result of the job, given by the master
data JobResult = JobResult {
    jr_JobId         :: ! JobId
  , jr_Output        :: [S.FileId]
  } deriving (Show)





-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

{-# NOINLINE globalJobId #-}
globalJobId :: MVar JobId
globalJobId = unsafePerformIO $ newMVar 0

nextJobId :: IO JobId
nextJobId 
  = do
    modifyMVar globalJobId $
      \i ->
      do
      let i' = i + 1
      return (i', i')


{-# NOINLINE globalTaskId #-}
globalTaskId :: MVar TaskId
globalTaskId = unsafePerformIO $ newMVar 0

nextTaskId :: IO TaskId
nextTaskId
  = do
    modifyMVar globalTaskId $
      \i ->
      do
      let i' = i + 1
      return (i', i')


-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

newJobData :: JobInfo -> IO JobData
newJobData info
  = do
    i <- nextJobId
    t <- getCurrentTime
    let outputMap = MMap.insertList JSIdle (ji_Input info) MMap.empty
    return (JobData i JSIdle outputMap info t t)
    
newTaskData :: JobId -> TaskType -> TaskState -> FunctionData -> FunctionName -> IO TaskData
newTaskData jid tt ts i a
  = do
    tid <- nextTaskId
    return (TaskData jid tid tt ts (Just i) Nothing a) 

    
data TaskSendResult = TSRSend | TSRNotSend | TSRError
  deriving (Show, Eq, Ord, Enum)    
    
-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

intersections :: Ord a => [Set.Set a] -> Set.Set a  
intersections [] = Set.empty
intersections (s:ss) = foldl Set.intersection s ss 

type TaskSendFunction = TaskData -> IO (TaskSendResult)


type JobMap         = Map.Map JobId JobData
type TaskMap        = Map.Map TaskId TaskData
    
type StateJobIdMap  = MMap.MultiMap JobState JobId

type JobIdTaskIdMap = MMap.MultiMap JobId TaskId
type TypeTaskIdMap  = MMap.MultiMap TaskType TaskId
type StateTaskIdMap = MMap.MultiMap TaskState TaskId
   
    
data JobControlFunctions = JobControlFunctions {
    jcf_TaskSend :: TaskSendFunction
  }
instance Show JobControlFunctions where
  show _ = "JobControlFunctions"

    
data JobControlData = JobControlData {
    jcd_Functions      :: JobControlFunctions
  , jcd_JobMap         :: ! JobMap
  , jcd_TaskMap        :: ! TaskMap
  , jcd_StateJobIdMap  :: ! StateJobIdMap
  , jcd_JobIdTaskIdMap :: ! JobIdTaskIdMap
  , jcd_TypeTaskIdMap  :: ! TypeTaskIdMap
  , jcd_StateTaskIdMap :: ! StateTaskIdMap
  } deriving (Show)


newJobControlData :: TaskSendFunction -> JobControlData
newJobControlData sf = JobControlData jcf jm tm sjm jtm ttm stm
  where
  jcf = JobControlFunctions sf
  jm = Map.empty
  tm = Map.empty
  sjm = MMap.empty
  jtm = MMap.empty
  ttm = MMap.empty
  stm = MMap.empty


createNewJob :: JobInfo -> JobControlData -> IO JobControlData
createNewJob ji jcd
  = do
    jd <- newJobData ji
    return (addJob jd jcd) 




addJob :: JobData -> JobControlData -> JobControlData
addJob jd jcd = jcd { jcd_JobMap = jm', jcd_StateJobIdMap = sjm' }
  where
  jm' = Map.insert (jd_JobId jd) jd (jcd_JobMap jcd)
  sjm' = MMap.insert (jd_State jd) (jd_JobId jd) (jcd_StateJobIdMap jcd)

updateJob :: JobData -> JobControlData -> JobControlData
updateJob jd jcd = addJob jd $ maybe (jcd) (\_ -> changeJobState jid js jcd) maybeOldJob
  where
    maybeOldJob = (Map.lookup jid (jcd_JobMap jcd))
    jid = jd_JobId jd
    js = jd_State jd
    

changeJobState :: JobId -> JobState -> JobControlData -> JobControlData
changeJobState jid js jcd = changeJobState' (Map.lookup jid (jcd_JobMap jcd))
  where
  changeJobState' (Nothing) = jcd
  changeJobState' (Just jd) = jcd { jcd_JobMap = jm', jcd_StateJobIdMap = sjm' }
    where
    jd' = jd { jd_State = js }  -- change JobData
    js' = jd_State jd           -- get old state
    jm' = Map.insert jid jd' (jcd_JobMap jcd) -- change JobData
    sjm' = MMap.insert js jid $ MMap.deleteElem js' jid (jcd_StateJobIdMap jcd) -- change StateJobIdMap

getJobIds :: [JobState] -> JobControlData -> [JobId]
getJobIds states jcd = Set.toList $ MMap.filterElements states (jcd_StateJobIdMap jcd)

  




addTask :: JobControlData -> TaskData -> JobControlData
addTask jcd td= jcd { jcd_TaskMap = tm', jcd_JobIdTaskIdMap = jtm', jcd_TypeTaskIdMap = ttm', jcd_StateTaskIdMap = stm' }
  where
    tid = td_TaskId td
    tm' = Map.insert tid td (jcd_TaskMap jcd)
    jtm' = MMap.insert (td_JobId td) tid (jcd_JobIdTaskIdMap jcd)
    ttm' = MMap.insert (td_Type td) tid (jcd_TypeTaskIdMap jcd)
    stm' = MMap.insert (td_State td) tid (jcd_StateTaskIdMap jcd)

changeTaskState :: TaskId -> TaskState -> JobControlData -> JobControlData
changeTaskState tid ts jcd = changeTaskState' (Map.lookup tid (jcd_TaskMap jcd))
  where
  changeTaskState' (Nothing) = jcd
  changeTaskState' (Just td) = jcd { jcd_TaskMap = tm', jcd_StateTaskIdMap = stm' }
    where
    td' = td { td_State = ts }  -- change TaskData
    ts' = td_State td           -- get old state
    tm' = Map.insert tid td' (jcd_TaskMap jcd) -- change TaskData
    stm' = MMap.insert ts tid $ MMap.deleteElem ts' tid (jcd_StateTaskIdMap jcd) -- change StateTaskIdMap

getTaskIds :: [JobId] -> [TaskType] -> [TaskState] -> JobControlData -> [TaskId]
getTaskIds jobs types states jcd = Set.toList $ intersections sets
  where
  jobSelected   = MMap.filterElements jobs (jcd_JobIdTaskIdMap jcd)
  typeSelected  = MMap.filterElements types (jcd_TypeTaskIdMap jcd)
  stateSelected = MMap.filterElements states (jcd_StateTaskIdMap jcd)
  sets = [jobSelected, typeSelected, stateSelected]


-- ----------------------------------------------------------------------------


-- type AssignTaskHook = [TaskId] -> IO [(TaskId, Bool)]

sendTask :: JobControlData -> TaskData -> IO JobControlData
sendTask jcd td
  = do
    let sendFunction = jcf_TaskSend $ jcd_Functions jcd
    sendResult <- sendFunction td
    case sendResult of
      (TSRSend) ->
        do
        let jcd' = toNextTaskState jcd td
        return jcd'
      (TSRNotSend) ->
        do
        return jcd
      (TSRError) ->
        do
        let jcd' = toErrorTaskState jcd td
        return jcd'
        

finishTask :: JobControlData -> TaskData -> JobControlData
finishTask jcd td = toNextTaskState jcd td
-- TODO record files to next job phase...
--  where
--    jd = fromJust $ Map.lookup (td_JobId td) (jcd_JobMap jcd)
--    output = td_Output td
--    inputMap' = MMap.insertSet (getNextJobState state) input inputMap 
--    
--    let 
--        let jd' = jd { jd_Inputs = inputMap' }
--        return $ updateJob jd' jcd
--    
--    jcd'' = 


processTasks :: JobControlData -> IO JobControlData
processTasks jcd
  = do
    putStrLn "processing Tasks:"
    -- get all runnning jobs (not idle...)
    let runningJobs = getJobIds [JSMap, JSCombine, JSReduce] jcd
    putStrLn $ "running Jobs:\n" ++ show runningJobs
    -- get all idle tasks
    let idleTasks = getTaskIds runningJobs [] [TSIdle] jcd
    putStrLn $ "idle Tasks:\n" ++ show idleTasks
    let idleTaskDatas = mapMaybe (\tid -> Map.lookup tid (jcd_TaskMap jcd)) idleTasks
    -- send all idle Tasks
    jcd' <- foldM sendTask jcd idleTaskDatas    
    -- get all completed Tasks
    let completedTasks = getTaskIds runningJobs [] [TSCompleted] jcd'
    let completedTaskDatas = mapMaybe (\tid -> Map.lookup tid (jcd_TaskMap jcd')) completedTasks
    -- inProgressTasks = getTaskIds runningJobs [] [TSInProgress] jcd
    let jcd'' = foldl finishTask jcd' completedTaskDatas
    return jcd''
    
    
    

-- ----------------------------------------------------------------------------

-- type AssignJobHook = [JobId] -> IO [(JobId, Bool)]



allTasksFinished :: JobControlData -> JobId -> Bool
allTasksFinished jcd jid = null notFinishedTasks
  where
    notFinishedTasks = getTaskIds [jid] [] [TSIdle, TSInProgress, TSCompleted] jcd


hasPhase :: JobData -> Bool
hasPhase jd = isJust $ getCurrentTaskAction jd

{-
getCurrentTaskInfo :: JobData -> (Maybe TaskAction, Maybe TaskType, InputPattern)
getCurrentTaskInfo jd = (a, t, i)
  where
  a = getCurrentTaskAction jd
  t = fromJobStatetoTaskType (jd_State jd)
  i = getCurrentTaskInput jd
-}

getCurrentTaskAction :: JobData -> Maybe FunctionName
getCurrentTaskAction jd = getTaskAction' (jd_Info jd) (jd_State jd)
  where
  getTaskAction' ji JSMap     = ji_MapAction ji
  getTaskAction' ji JSCombine = ji_CombineAction ji
  getTaskAction' ji JSReduce  = ji_ReduceAction ji 
  getTaskAction' _  _         = Nothing

-- getCurrentTaskInput :: JobData -> InputPattern
-- getCurrentTaskInput = undefined

fromJobStatetoTaskType :: JobState -> Maybe TaskType
fromJobStatetoTaskType JSMap     = Just TTMap
fromJobStatetoTaskType JSCombine = Just TTCombine
fromJobStatetoTaskType JSReduce  = Just TTReduce
fromJobStatetoTaskType _         = Nothing


createTasks :: JobControlData -> JobData -> IO JobControlData
createTasks jcd jd
  = do
    let state = jd_State jd
    let outputMap = (jd_OutputMap jd)
    -- our input is the output of the previous state
    let input = MMap.lookup (getPrevJobState state) outputMap
    if (hasPhase jd)
      then do
        -- TODO do partitioning here and better file naming
        let inputList = Set.toList input
        let jid = jd_JobId jd
        let a = fromJust $ getCurrentTaskAction jd
        let tt  = fromJust $ fromJobStatetoTaskType state
        -- create new tasks
        taskDatas <- mapM (\i -> newTaskData jid tt TSIdle i a) inputList
        -- add task to controller
        let jcd' = foldl addTask jcd taskDatas        
        return jcd'
      else do
        -- there is no action in this state, so we copy the input to the output
        let outputMap' = MMap.insertSet state input outputMap 
        let jd' = jd { jd_OutputMap = outputMap' }
        return $ updateJob jd' jcd
 

getNextTaskState :: TaskState -> TaskState
getNextTaskState TSError    = TSError
getNextTaskState TSFinished = TSFinished
getNextTaskState s          = succ s

toNextTaskState :: JobControlData -> TaskData -> JobControlData
toNextTaskState jcd td = changeTaskState (td_TaskId td) (getNextTaskState (td_State td)) jcd 

toErrorTaskState :: JobControlData -> TaskData -> JobControlData
toErrorTaskState jcd td = changeTaskState (td_TaskId td) TSError jcd

getNextJobState :: JobState -> JobState
getNextJobState JSError    = JSError
getNextJobState JSFinished = JSFinished
getNextJobState s          = succ s

getPrevJobState :: JobState -> JobState
getPrevJobState JSIdle  = JSIdle
getPrevJobState JSError = JSError
getPrevJobState s       = pred s

toNextJobState :: JobControlData -> JobData -> JobControlData
toNextJobState jcd jd = changeJobState (jd_JobId jd) (getNextJobState (jd_State jd)) jcd

processJobs :: JobControlData -> IO JobControlData
processJobs jcd
  = do
    -- get all working jobs (idle or running)
    putStrLn "processing Jobs"
    let workingJobs = getJobIds [JSIdle, JSMap, JSCombine, JSReduce] jcd
    putStrLn $ "working jobs:\n" ++ show workingJobs
    -- process only jobs, whose current phase is done
    let jobsWithoutTasks = filter (allTasksFinished jcd) workingJobs
    putStrLn $ "jobsWithoutTasks:\n" ++ show jobsWithoutTasks
    -- get the old jobdatas
    let oldJobDatas = mapMaybe (\jid -> Map.lookup jid (jcd_JobMap jcd)) jobsWithoutTasks
    putStrLn $ "oldJobDatas:\n" ++ show oldJobDatas
    -- change the job states to the next phase
    let jcd' = foldl toNextJobState jcd oldJobDatas
    -- get the new jobdatas
    let newJobDatas = mapMaybe (\jid -> Map.lookup jid (jcd_JobMap jcd')) jobsWithoutTasks
    putStrLn $ "newJobDatas:\n" ++ show newJobDatas
    -- create new tasks for each Job
    jcd'' <- foldM createTasks jcd' newJobDatas
    return jcd''
    
-- ----------------------------------------------------------------------------

doControlling :: JobControlData -> IO (JobControlData)
doControlling jcd
  = do
    processTasks jcd >>= processJobs
    
    
  {-
    -- assign all idle tasks from all running jobs to InProgress
    let runningJobs = getJobIds [JSMap, JSCombine, JSReduce] jcd 
    let idleTasks   = getTaskIds runningJobs [] [TSIdle] jcd    
   
    
    -- assign all completed tasks from all running jobs to Finished
    let completedTasks = getTaskIds runningJobs [] [TSCompleted] jcd1
    jcd2 <- assignTasksToCompleted completedTasks jcd1
  
    -- assign idle jobs to map-phase
    let idleJobs = getJobIds [JSIdle] jcd2
    jcd3 <- assignJobsToMap idleJobs jcd2
    
    -- assign completed map-phase to combine-phase
    let mapJobs = getJobIds [JSMap] jcd3
    assignJobsToCombine
    
    assignJobsToReduce
    
    -- assign completed combine-phase to reduce-phase
    
    -- assign completed reduce-phase to finished
      
    threadDelay 1000000 -- 1.0 sec
    doJobControlling jcd''
-}






{-    
addJob :: JobData -> MasterMaps -> MasterMaps
addJob jd mm = mm { mm_JobMap = jm' }
  where
    jid = jd_JobId jd
    jm  = mm_JobMap mm 
    jm' = Map.insert jid jd jm

-- anderen zuweisen und inprogress setzen
assignTasks :: Set.Set TaskId -> MasterMaps -> IO MasterMaps
assignTasks = undefined

-- anderen entziehen und finished
setFinished :: Set.Set TaskId -> MasterMaps -> IO MasterMaps
setFinished = undefined

createCombiners :: Set.Set TaskId -> MasterMaps -> IO MasterMaps
createCombiners = undefined

allTypes :: [TaskType]
allTypes = undefined

splitByCombinerType :: [JobId] -> ([JobId], [JobId])
splitByCombinerType = undefined

splitByReducerType :: [JobId] -> ([JobId], [JobId])
splitByReducerType = undefined

startCombiners :: MasterMaps -> IO (MasterMaps)
startCombiners mm
  = do
    -- handle the jobs with single combiners
    let allJobs = getJobIds [JSIdle] [CTSingle] mm
    let mapCompleted = getTaskIds allJobs [TTMap] [TSCompleted] mm
    sequence $ map (startSingleCombiner) mapCompleted
    
    -- handle the jobs with multiple combiners
    let allJobs = getJobIds [JSIdle] [CTMultiple] mm
    
    for every Job... Partition Combiners
    let mapNotFinished = getTaskIds myJob [TTMap] [TSIdle, TSInProgress, TSCompleted] mm
    let mapFinished = getTaskIds myJob [TTMap] [TSFinished] mm
    
    -- if (null mapNotFinished) then
    --   partition and set combiners to idle
    
    return mm

startReducers :: MasterMaps -> IO (MasterMaps)
startReducers mm
  = do
    -- handle jobs with no combiners
-}

    
    
    
    
    
{-
    
type JobMap = Map.Map JobId JobData
type JobQueue = [JobId]
type TaskMap = Map.Map TaskId TaskData
type TaskStateMap = (Map.Map TaskId TaskState, Set.Set TaskId, Set.Set TaskId, Set.Set TaskId, Set.Set TaskId)


-}