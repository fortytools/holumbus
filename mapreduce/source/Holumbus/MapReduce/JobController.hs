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

  TaskSendResult(..)
, TaskSendFunction

, JobController

, printJobController

-- * Creation / Destruction
, newJobController
, closeJobController
, setTaskSendHook
, setMapActions
, setReduceActions

-- * Job Controller 
, startJobController
, stopJobController
, singleStepJobControlling

-- * performing MapReduce-Jobs
, startJob
, stopJob
, performJob

-- * handling Task-Responses
, setTaskCompleted
, setTaskError
)
where

import qualified Control.Exception as E
import           Control.Concurrent
import           Control.Monad
import           Data.Binary
import           Data.Maybe
import           Data.Time
import           Data.Typeable
import qualified Data.Map as Map
import qualified Data.Set as Set

import           Holumbus.MapReduce.Types
import           Holumbus.FileSystem.Storage as S
import qualified Holumbus.MapReduce.AccuMap as AMap
import qualified Holumbus.MapReduce.MultiMap as MMap


-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


newJobData :: JobControllerData -> JobInfo -> IO (JobControllerData, JobData, MVar JobResult)
newJobData jcd info
  = do
    t <- getCurrentTime
    mVar <- newEmptyMVar
    let jrc = JobResultContainer mVar 
    let jid = jcd_NextJobId jcd
    let jcd' = jcd { jcd_NextJobId = (jid+1) }
    let bout = encodeTupleList (ji_Input info)
    --TODO
    let outputMap = AMap.insertList JSIdle [(1,bout)] AMap.empty
    return (jcd', JobData jid JSIdle outputMap info t t jrc, mVar)


newTaskData 
  :: JobControllerData 
  -> JobId -> TaskType -> TaskState -> [FunctionData] -> TaskAction 
  -> IO (JobControllerData, TaskData)
newTaskData jcd jid tt ts i a
  = do
    let tid = jcd_NextTaskId jcd
    let jcd' = jcd { jcd_NextTaskId = (tid+1) }
    return (jcd', TaskData jid tid tt ts i [] a) 
    
data TaskSendResult = TSRSend | TSRNotSend | TSRError
  deriving (Show, Eq, Ord, Enum)    
    
-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

intersections :: Ord a => [Set.Set a] -> Set.Set a  
intersections [] = Set.empty
intersections (s:ss) = foldl Set.intersection s ss 


mapAccumLM
  :: (Monad m) 
  => (acc -> x -> m (acc, y)) -- Function of of input list
                               -- and accumulator, returning new
                               -- accumulator and of result list
  -> acc                       -- Initial accumulator 
  -> [x]                       -- Input list
  -> m (acc, [y])             -- Final accumulator and result list
mapAccumLM _ s [] = return (s, [])
mapAccumLM f s (x:xs) 
  = do
    (s', y ) <- f s x
    (s'',ys) <- mapAccumLM f s' xs
    return (s'',y:ys)




type TaskSendFunction = TaskData -> IO (TaskSendResult)

dummyTaskSendFunction :: TaskData -> IO (TaskSendResult)
dummyTaskSendFunction _ = return TSRSend


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

    
data JobControllerData = JobControllerData {
  -- internal
    jcd_ServerThreadId :: Maybe ThreadId
  , jcd_ServerDelay    :: Int
  -- control
  -- , jcd_MaxRunningJobs :: Int
  , jcd_NextJobId      :: JobId
  , jcd_NextTaskId     :: TaskId
  , jcd_Functions      :: JobControlFunctions
  
  , jcd_MapActionMap   :: MapActionMap
  , jcd_ReduceActionMap :: ReduceActionMap
  
  -- job control
  , jcd_JobMap         :: ! JobMap
  , jcd_TaskMap        :: ! TaskMap
  , jcd_StateJobIdMap  :: ! StateJobIdMap
  , jcd_JobIdTaskIdMap :: ! JobIdTaskIdMap
  , jcd_TypeTaskIdMap  :: ! TypeTaskIdMap
  , jcd_StateTaskIdMap :: ! StateTaskIdMap
  } deriving (Show)




type JobController = MVar JobControllerData

printJobController :: JobController -> IO String
printJobController jc
  = withMVar jc $ \jcd -> return $ show jcd


data JobControllerException
  = KillServerException
  deriving (Show, Typeable)

-- ----------------------------------------------------------------------------
-- Contruction / Destruction 
-- ----------------------------------------------------------------------------

defaultJobControllerData :: JobControllerData
defaultJobControllerData = jcd
  where
  jcd = JobControllerData
    Nothing
    1000000
    1
    1
    jcf
    Map.empty
    Map.empty
    -- emptyPartitionFunctionMap
    Map.empty
    Map.empty
    MMap.empty
    MMap.empty
    MMap.empty
    MMap.empty
  jcf = JobControlFunctions
    dummyTaskSendFunction

newJobController :: IO JobController
newJobController
  = do
    let jcd = defaultJobControllerData
    jc <- newMVar jcd
    return jc 

closeJobController :: JobController -> IO ()
closeJobController jc
  = do
    stopJobController jc


setTaskSendHook :: TaskSendFunction -> JobController -> IO ()
setTaskSendHook f jc
  = do
    modifyMVar jc $
      \jcd ->
      do
      let funs = jcd_Functions jcd
      let funs' = funs { jcf_TaskSend = f }
      return (jcd { jcd_Functions = funs' }, ())


setMapActions :: MapActionMap -> JobController -> IO ()
setMapActions mm jc
  = modifyMVar jc $ \jcd -> return (jcd {jcd_MapActionMap = mm }, ())


setReduceActions :: ReduceActionMap -> JobController -> IO ()
setReduceActions rm jc
  = modifyMVar jc $ \jcd -> return (jcd {jcd_ReduceActionMap = rm }, ())


-- ----------------------------------------------------------------------------
-- server functions
-- ----------------------------------------------------------------------------

startJobController :: JobController -> IO ()
startJobController jc
  = do
    modifyMVar jc $ 
      \jcd -> 
      do
      thd <- case (jcd_ServerThreadId jcd) of
        (Just i) -> return i
        (Nothing) ->
          do
          i <- forkIO $ doProcessing jc True
          return i
      return (jcd {jcd_ServerThreadId = (Just thd)}, ())



stopJobController :: JobController -> IO ()
stopJobController jc
  = do
    modifyMVar jc $ 
      \jcd -> 
      do
      case (jcd_ServerThreadId jcd) of
        (Nothing) -> return ()
        (Just i) -> 
          do
          E.throwDynTo i KillServerException
          yield
          return ()
      return (jcd {jcd_ServerThreadId = Nothing}, ())


singleStepJobControlling :: JobController -> IO ()
singleStepJobControlling jc
  = do
    singleStepAllowed <- withMVar jc $ 
      \jcd -> 
      do
      case (jcd_ServerThreadId jcd) of
        (Nothing) -> return True
        (Just _)  -> return False
    if singleStepAllowed then doProcessing jc False else return ()

-- ----------------------------------------------------------------------------
-- private functions
-- ----------------------------------------------------------------------------

-- | get the JobIds, they can be filtered by JobState, if the filter is empty
--   all Jobs are returned.
getJobIds :: [JobState] -> JobControllerData -> [JobId]
getJobIds states jcd = Set.toList $ MMap.filterElements states (jcd_StateJobIdMap jcd)


-- | get the TaskIds, they can be filtered by JobId, TaskType and TaskState, if
--   a filter is empty, this attribute is not taken into account
getTaskIds :: [JobId] -> [TaskType] -> [TaskState] -> JobControllerData -> [TaskId]
getTaskIds jobs types states jcd = Set.toList $ intersections sets
  where
  jobSelected   = MMap.filterElements jobs (jcd_JobIdTaskIdMap jcd)
  typeSelected  = MMap.filterElements types (jcd_TypeTaskIdMap jcd)
  stateSelected = MMap.filterElements states (jcd_StateTaskIdMap jcd)
  sets = [jobSelected, typeSelected, stateSelected]


allTasksFinished :: JobControllerData -> JobId -> Bool
allTasksFinished jcd jid = null notFinishedTasks
  where
    notFinishedTasks = getTaskIds [jid] [] [TSIdle, TSInProgress, TSCompleted] jcd



-- | adds a Job to the JobControllerData for execution
addJob :: JobData -> JobControllerData -> JobControllerData
addJob jd jcd = jcd { jcd_JobMap = jm', jcd_StateJobIdMap = sjm' }
  where
  jm' = Map.insert (jd_JobId jd) jd (jcd_JobMap jcd)
  sjm' = MMap.insert (jd_State jd) (jd_JobId jd) (jcd_StateJobIdMap jcd)


changeJobState :: JobId -> JobState -> JobControllerData -> JobControllerData
changeJobState jid js jcd = changeJobState' (Map.lookup jid (jcd_JobMap jcd))
  where
  changeJobState' (Nothing) = jcd
  changeJobState' (Just jd) = jcd { jcd_JobMap = jm', jcd_StateJobIdMap = sjm' }
    where
    jd' = jd { jd_State = js }  -- change JobData
    js' = jd_State jd           -- get old state
    jm' = Map.insert jid jd' (jcd_JobMap jcd) -- change JobData
    sjm' = MMap.insert js jid $ MMap.deleteElem js' jid (jcd_StateJobIdMap jcd) -- change StateJobIdMap


updateJob :: JobData -> JobControllerData -> JobControllerData
updateJob jd jcd = addJob jd $ maybe (jcd) (\_ -> changeJobState jid js jcd) maybeOldJob
  where
    maybeOldJob = (Map.lookup jid (jcd_JobMap jcd))
    jid = jd_JobId jd
    js = jd_State jd



addTask :: JobControllerData -> TaskData -> JobControllerData
addTask jcd td= jcd { jcd_TaskMap = tm', jcd_JobIdTaskIdMap = jtm', jcd_TypeTaskIdMap = ttm', jcd_StateTaskIdMap = stm' }
  where
    tid = td_TaskId td
    tm' = Map.insert tid td (jcd_TaskMap jcd)
    jtm' = MMap.insert (td_JobId td) tid (jcd_JobIdTaskIdMap jcd)
    ttm' = MMap.insert (td_Type td) tid (jcd_TypeTaskIdMap jcd)
    stm' = MMap.insert (td_State td) tid (jcd_StateTaskIdMap jcd)


changeTaskState :: TaskId -> TaskState -> JobControllerData -> JobControllerData
changeTaskState tid ts jcd = changeTaskState' (Map.lookup tid (jcd_TaskMap jcd))
  where
  changeTaskState' (Nothing) = jcd
  changeTaskState' (Just td) = jcd { jcd_TaskMap = tm', jcd_StateTaskIdMap = stm' }
    where
    td' = td { td_State = ts }  -- change TaskData
    ts' = td_State td           -- get old state
    tm' = Map.insert tid td' (jcd_TaskMap jcd) -- change TaskData
    stm' = MMap.insert ts tid $ MMap.deleteElem ts' tid (jcd_StateTaskIdMap jcd) -- change StateTaskIdMap


updateTaskOutput :: TaskId -> [(Int, [FunctionData])] -> JobControllerData -> JobControllerData
updateTaskOutput _ [] jcd = jcd
updateTaskOutput tid o jcd = updateTaskOuput' (Map.lookup tid (jcd_TaskMap jcd))
  where
  updateTaskOuput' (Nothing) = jcd
  updateTaskOuput' (Just td) = jcd { jcd_TaskMap = tm' }
    where
    td' = td { td_Output = o }  -- change TaskData
    tm' = Map.insert tid td' (jcd_TaskMap jcd) -- change TaskData

-- ----------------------------------------------------------------------------
-- Info an Debug
-- ----------------------------------------------------------------------------


-- ----------------------------------------------------------------------------
-- Task Controlling
-- ----------------------------------------------------------------------------


startJob :: JobInfo -> JobController -> IO (Either String (JobId, MVar JobResult))
startJob ji jc
  = do
    modifyMVar jc $
      \jcd ->
      do
      -- test the job info... we don't want false jobs
      let (b, m) = testJobInfo ji (jcd_MapActionMap jcd) (jcd_ReduceActionMap jcd)
      if b 
        then do
          (jcd',jd, jr) <- newJobData jcd ji
          return (addJob jd jcd', Right (jd_JobId jd, jr))
        else do
          return (jcd, Left m)


stopJob :: JobId -> JobController -> IO ()
stopJob _ _
  = do
    undefined

performJob :: JobInfo -> JobController -> IO (Either String JobResult)
performJob ji jc
  = do
    -- start the Job
    res <- startJob ji jc
    case res of
      (Left m) -> return $ Left m
      (Right (_, mjr)) ->
        do
        -- wait until the result is ready
        r <- withMVar mjr $ \jr -> return jr
        return $ Right r

-- ----------------------------------------------------------------------------
-- Task Processing
-- ----------------------------------------------------------------------------


doProcessing :: JobController -> Bool -> IO ()
doProcessing jc loop
  = do
    E.catchDyn (doProcessing' jc loop)
      handler
    where
      handler :: JobControllerException -> IO ()
      handler err = putStrLn (show err)
      doProcessing' jc' loop'
        = do
          handleTasks jc'
          handleJobs jc'
          if loop' 
            then do
              delay <- withMVar jc' (\jcd -> return $ jcd_ServerDelay jcd)
              threadDelay delay
              doProcessing' jc' loop'
            else
              return ()



-- ----------------------------------------------------------------------------
-- Process Tasks
-- ----------------------------------------------------------------------------

toNextTaskState :: JobControllerData -> TaskData -> JobControllerData
toNextTaskState jcd td = changeTaskState (td_TaskId td) (getNextTaskState (td_State td)) jcd 


sendTask :: JobControllerData -> TaskData -> IO JobControllerData
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


finishTask :: JobControllerData -> TaskData -> JobControllerData
finishTask jcd td = toNextTaskState jcd' td
  where
    jd = fromJust $ Map.lookup (td_JobId td) (jcd_JobMap jcd)
    outputMap = AMap.insertList (jd_State jd) (td_Output td) (jd_OutputMap jd) 
    jcd' = updateJob (jd {jd_OutputMap = outputMap}) jcd 


handleTasks :: JobController -> IO ()
handleTasks jc
  = modifyMVar jc $
      \jcd ->
      do
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
      return (jcd'',())


-- ----------------------------------------------------------------------------
-- Process Jobs
-- ----------------------------------------------------------------------------

toNextJobState :: JobControllerData -> JobData -> JobControllerData
toNextJobState jcd jd = changeJobState (jd_JobId jd) (getNextJobState (jd_State jd)) jcd

toErrorTaskState :: JobControllerData -> TaskData -> JobControllerData
toErrorTaskState jcd td = changeTaskState (td_TaskId td) TSError jcd

toCompletedTaskState :: JobControllerData -> TaskData -> JobControllerData
toCompletedTaskState jcd td = updateTaskOutput tid o $ changeTaskState tid TSCompleted jcd
  where
  tid = td_TaskId td
  o = td_Output td
  

hasPhase :: JobData -> Bool
hasPhase jd = isJust $ getCurrentTaskAction jd


getCurrentTaskAction :: JobData -> Maybe TaskAction
getCurrentTaskAction jd = getTaskAction' (jd_Info jd) (jd_State jd)
  where
  getTaskAction' ji JSMap     = ji_MapAction ji
  getTaskAction' ji JSCombine = ji_CombineAction ji
  getTaskAction' ji JSReduce  = ji_ReduceAction ji 
  getTaskAction' _  _         = Nothing


{-
performPartition :: JobControllerData -> JobData -> [FunctionData] -> IO [FunctionData]
performPartition jcd jd bin
  = do
    let pmap = jcd_PartitionMap jcd
    let fn = getCurrentPartitionFunction jd
    let maybeP = dispatchPartitionFunction pmap fn
    let p = maybe (\t -> return t) id maybeP
    bout <- p bin
    return bout
-}

createTasks :: JobControllerData -> JobData -> IO JobControllerData
createTasks jcd jd
  = do
    let state = jd_State jd
    let outputMap = (jd_OutputMap jd)
    -- our input is the output of the previous state
    let inputList = AMap.lookup (getPrevJobState state) outputMap
    if (hasPhase jd)
      then do
        -- TODO do partitioning here and better file naming
        -- sortedInputs <- performPartition jcd jd inputList
        
        let jid = jd_JobId jd
        let a = fromJust $ getCurrentTaskAction jd
        let tt  = fromJust $ fromJobStatetoTaskType state
        -- create new tasks
        -- TODO
        (jcd', taskDatas) <- mapAccumLM (\d (_,i) -> newTaskData d jid tt TSIdle i a) jcd inputList
        -- taskDatas <- mapM (\i -> newTaskData jid tt TSIdle i a) inputList
        -- add task to controller
        let jcd'' = foldl addTask jcd' taskDatas        
        return jcd''
      else do
        -- there is no action in this state, so we copy the input to the output
        let outputMap' = AMap.insertList state inputList outputMap 
        let jd' = jd { jd_OutputMap = outputMap' }
        return $ updateJob jd' jcd


createResults :: JobControllerData -> JobData -> IO JobControllerData
createResults jcd jd
  = do
    let outputList = AMap.lookup JSCompleted (jd_OutputMap jd)
    let (JobResultContainer mVarResult) = jd_Result jd
    let res = JobResult $ concat $ map (snd) outputList
    putMVar mVarResult res
    return jcd  


handleJobs :: JobController -> IO ()
handleJobs jc
  = modifyMVar jc $
      \jcd ->
      do
      -- get all working jobs (idle or running)
      putStrLn "processing Jobs"
      let workingJobs = getJobIds [JSIdle, JSMap, JSCombine, JSReduce] jcd
      putStrLn $ "working jobs:\n" ++ show workingJobs
      -- process only jobs, whose current phase is done
      let jobsWithoutTasks = filter (allTasksFinished jcd) workingJobs
      putStrLn $ "jobsWithoutTasks:\n" ++ show jobsWithoutTasks
      -- get the old jobdatas
      let oldJobDatas = mapMaybe (\jid -> Map.lookup jid (jcd_JobMap jcd)) jobsWithoutTasks
      -- putStrLn $ "oldJobDatas:\n" ++ show oldJobDatas
      -- change the job states to the next phase
      let jcd1 = foldl toNextJobState jcd oldJobDatas
      -- get the new jobdatas
      let newJobDatas = mapMaybe (\jid -> Map.lookup jid (jcd_JobMap jcd1)) jobsWithoutTasks
      -- putStrLn $ "newJobDatas:\n" ++ show newJobDatas
      -- create new tasks for each Job
      jcd2 <- foldM createTasks jcd1 newJobDatas
      
      -- get all completed Jobs
      let completedJobs = getJobIds [JSCompleted] jcd2
      putStrLn $ "completed Jobs:\n" ++ show completedJobs
      -- get the completed JobDatas
      let completedJobDatas = mapMaybe (\jid -> Map.lookup jid (jcd_JobMap jcd2)) completedJobs
      -- change the job states to the next phase
      let jcd3 = foldl toNextJobState jcd2 completedJobDatas
      -- get the new jobdatas
      let finishedJobDatas = mapMaybe (\jid -> Map.lookup jid (jcd_JobMap jcd3)) completedJobs
      -- create new tasks for each Job
      jcd4 <- foldM createResults jcd3 finishedJobDatas
      
      return (jcd4, ())




-- ----------------------------------------------------------------------------   
-- handling Task-Responses
-- ----------------------------------------------------------------------------

setTaskCompleted :: JobController -> TaskData -> IO ()
setTaskCompleted jc td
  = do
    putStrLn "JobController: setTaskCompleted waiting..."
    modifyMVar jc $
      \jcd ->
      do
      putStrLn "JobController: setTaskCompleted setting..."
      let jcd' = toCompletedTaskState jcd td
      return (jcd', ())

    
setTaskError :: JobController -> TaskData -> IO ()
setTaskError jc td
  = do
    modifyMVar jc $
      \jcd ->
      do
      let jcd' = toErrorTaskState jcd td
      return (jcd', ())
    