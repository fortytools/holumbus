-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.MapReduce.TaskProcessor
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts #-}
module Holumbus.MapReduce.TaskProcessor
(
-- * Datatypes
  TaskResultFunction
, TaskProcessor


-- * Creation / Destruction
, newTaskProcessor
, closeTaskProcessor
, setMapFunctionMap
, setReduceFunctionMap


-- * Info an Debug
, listTaskIds 
, getMapFunctions
, getReduceFunctions


-- * Task Creation / Destruction
, startTask
, stopTask
, stopAllTasks 
)
where

import qualified Control.Exception as E
import Control.Concurrent
import Data.Binary
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Typeable

import Holumbus.MapReduce.Types
import Holumbus.MapReduce.JobController

import Holumbus.FileSystem.FileSystem as F
import Holumbus.FileSystem.Storage as S





-- ----------------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------------

-- | a function for responding a
type TaskResultFunction = TaskData -> IO Bool

dummyTaskResultFunction :: TaskData -> IO Bool
dummyTaskResultFunction _ = return True 

data TaskProcessorFunctions = TaskProcessorFunctions {
    tpf_TaskCompleted :: TaskResultFunction
  , tpf_TaskError     :: TaskResultFunction
  }


instance Show TaskProcessorFunctions where
  show _ = "{TaskProcessorFunctions}"


data TaskException
  = KillTaskException
  | UnkownTaskException
  deriving (Show, Typeable)


-- | data, needed by the MapReduce-System to run the tasks
data TaskProcessorData = TaskProcessorData {
  -- internal
    tpd_ServerThreadId    :: Maybe ThreadId
  , tpd_ServerDelay       :: Int
  -- configuration
  , tpd_MaxTasks          :: Int
  , tpd_Functions         :: TaskProcessorFunctions
  , tpd_MapFunctionMap    :: MapFunctionMap
  , tpd_ReduceFunctionMap :: ReduceFunctionMap
  -- task processing
  , tpd_TaskQueue         :: [TaskData]
  , tpd_CompletedTasks    :: Set.Set TaskData
  , tpd_ErrorTasks        :: Set.Set TaskData
  , tpd_TaskIdThreadMap   :: Map.Map TaskId ThreadId
  
  }

type TaskProcessor = MVar TaskProcessorData



-- ----------------------------------------------------------------------------
-- Creation / Destruction
-- ----------------------------------------------------------------------------

defaultTaskProcessorData :: TaskProcessorData
defaultTaskProcessorData = tpd
  where
    funs = TaskProcessorFunctions 
      dummyTaskResultFunction
      dummyTaskResultFunction
    tpd = TaskProcessorData
      Nothing
      1000000 -- one second delay
      1
      funs
      Map.empty
      Map.empty
      []
      Set.empty
      Set.empty
      Map.empty      


-- | creates a new TaskProcessor
newTaskProcessor :: IO TaskProcessor
newTaskProcessor
  = do
    let tpd = defaultTaskProcessorData
    tp <- newMVar tpd
    startTaskProcessor tp
    return tp
  -- TaskProcessorData fs emptyMapFunctionMap emptyReduceFunctionMap

closeTaskProcessor :: TaskProcessor -> IO ()
closeTaskProcessor tp
  = do
    stopTaskProcessor tp



-- | adds a MapFunction to the TaskProcessor
setMapFunctionMap :: MapFunctionMap -> TaskProcessor -> IO ()
setMapFunctionMap m tp
  = modifyMVar tp $
    \tpd -> return $ (tpd { tpd_MapFunctionMap = m }, ())


-- | adds a ReduceFunction to the TaskProcessor
setReduceFunctionMap :: ReduceFunctionMap -> TaskProcessor -> IO ()
setReduceFunctionMap m tp
  = modifyMVar tp $
      \tpd -> return $ (tpd { tpd_ReduceFunctionMap = m }, ())


-- ----------------------------------------------------------------------------
-- server functions
-- ----------------------------------------------------------------------------

startTaskProcessor :: TaskProcessor -> IO ()
startTaskProcessor tp
  = do
    modifyMVar tp $ 
      \ tpd -> 
      do
      thd <- case (tpd_ServerThreadId tpd) of
        (Just i) -> return i
        (Nothing) ->
          do
          i <- forkIO $ doProcessing tp
          return i
      return (tpd {tpd_ServerThreadId = (Just thd)}, ())



stopTaskProcessor :: TaskProcessor -> IO ()
stopTaskProcessor tp
  = do
    modifyMVar tp $ 
      \ tpd -> 
      do
      case (tpd_ServerThreadId tpd) of
        (Nothing) -> return ()
        (Just i) -> 
          do
          E.throwDynTo i myThreadId
          yield
          return ()
      return (tpd {tpd_ServerThreadId = Nothing}, ())


-- ----------------------------------------------------------------------------
-- private functions
-- ----------------------------------------------------------------------------

containsTask :: TaskId -> TaskProcessorData -> Bool
containsTask tid tpd = isTaskRunning || isTaskQueued
  where
  isTaskRunning = Map.member tid (tpd_TaskIdThreadMap tpd)
  isTaskQueued = any (\td -> (td_TaskId td) == tid) (tpd_TaskQueue tpd)


queueTask :: TaskData -> TaskProcessorData -> TaskProcessorData
queueTask td tpd = tpd { tpd_TaskQueue = q' }
  where
  q = tpd_TaskQueue tpd
  q' = q ++ [td]

  
dequeueTask :: TaskId -> TaskProcessorData -> TaskProcessorData
dequeueTask tid tpd = tpd { tpd_TaskQueue = q' }
  where
  q = tpd_TaskQueue tpd
  q' = filter (\td -> (td_TaskId td) /= tid) q 


getTaskThreadId :: TaskId -> TaskProcessorData -> Maybe ThreadId
getTaskThreadId tid tpd = Map.lookup tid (tpd_TaskIdThreadMap tpd)


getTasksIds :: TaskProcessorData -> [TaskId]
getTasksIds tpd = Set.toList $ Set.union (Set.fromList qs) (Set.fromList ts) 
  where
    qs = map (\td -> td_TaskId td) (tpd_TaskQueue tpd)
    ts = Map.keys (tpd_TaskIdThreadMap tpd)


addTask :: TaskData -> TaskProcessorData -> TaskProcessorData
addTask td tpd = if containsTask tid tpd then tpd else queueTask td tpd
  where
  tid = td_TaskId td


deleteTask :: TaskId -> TaskProcessorData -> TaskProcessorData
deleteTask tid tpd = dequeueTask tid tpd'
  where
  tpd' = tpd { tpd_TaskIdThreadMap = ttm' }
  ttm = tpd_TaskIdThreadMap tpd
  ttm' = Map.delete tid ttm



-- ----------------------------------------------------------------------------
-- Info an Debug
-- ----------------------------------------------------------------------------


listTaskIds :: TaskProcessor -> IO [TaskId] 
listTaskIds tp
  = withMVar tp $
      \tpd -> return $ getTasksIds tpd


-- | Lists all Map-Functions with Name, Descrition and Type
getMapFunctions :: TaskProcessor -> IO [(FunctionName, FunctionDescription, TypeRep)]
getMapFunctions tp
  = withMVar tp $
      \tpd -> 
      do
      let m = tpd_MapFunctionMap tpd
      let ls = map (\(n,d,t,_)->(n,d,t)) (Map.elems m)
      return ls


-- | Lists all Reduce-Functions with Name, Descrition and Type
getReduceFunctions :: TaskProcessor -> IO [(FunctionName, FunctionDescription, TypeRep)]
getReduceFunctions tp 
  = withMVar tp $
      \tpd -> 
      do
      let m = tpd_ReduceFunctionMap tpd
      let ls = map (\(n,d,t,_)->(n,d,t)) (Map.elems m)
      return ls



-- ----------------------------------------------------------------------------
-- Task Controlling
-- ----------------------------------------------------------------------------


-- | adds a Task to the TaskProcessor, the execution might be later
startTask :: TaskData -> TaskProcessor -> IO ()
startTask td tp
  = modifyMVar tp $
    \tpd-> do return (addTask td tpd, ())
      
  
stopTask :: TaskId -> TaskProcessor -> IO ()
stopTask tid tp
  = do
    mthd <- modifyMVar tp $
      \tpd-> 
      do
      let thd = getTaskThreadId tid tpd 
      return (deleteTask tid tpd, thd)
    maybe (return ()) (\thd -> E.throwDynTo thd KillTaskException) mthd


stopAllTasks :: TaskProcessor -> IO () 
stopAllTasks tp
  = do
    tids <- withMVar tp $ \tpd -> return $ getTasksIds tpd
    mapM (\tid -> stopTask tid tp) tids
    return ()





-- ----------------------------------------------------------------------------
-- Task Processing
-- ----------------------------------------------------------------------------

setTaskCompleted :: TaskData -> TaskProcessorData -> TaskProcessorData
setTaskCompleted td tpd = tpd { tpd_TaskIdThreadMap = ttm', tpd_CompletedTasks = ct' }
  where
  tid = td_TaskId td
  ttm' = Map.delete tid (tpd_TaskIdThreadMap tpd)
  ct' = Set.insert td (tpd_CompletedTasks tpd)


setTaskError :: TaskData -> TaskProcessorData -> TaskProcessorData
setTaskError td tpd = tpd { tpd_TaskIdThreadMap = ttm', tpd_ErrorTasks = et' }
  where
  tid = td_TaskId td
  ttm' = Map.delete tid (tpd_TaskIdThreadMap tpd)
  et' = Set.insert td (tpd_ErrorTasks tpd)


-- | mark the task as error and invoke the reply function
reportErrorTask :: TaskData -> TaskProcessor -> IO ()
reportErrorTask td tp
  = modifyMVar tp $
      \tpd -> 
      do 
      let tpd' = setTaskError td tpd
      let f = tpf_TaskError $ tpd_Functions tpd'
      f td
      return (tpd', ())


-- | mark the task as completed and invoke the reply function
reportCompletedTask :: TaskData -> TaskProcessor -> IO ()
reportCompletedTask td tp
  = modifyMVar tp $
      \tpd -> 
      do 
      let tpd' = setTaskCompleted td tpd
      let f = tpf_TaskCompleted $ tpd_Functions tpd'
      f td
      return (tpd', ())


setTaskRunning :: TaskId -> ThreadId -> TaskProcessorData -> TaskProcessorData
setTaskRunning tid thd tpd = tpd { tpd_TaskIdThreadMap = ttm' }
  where
  ttm = tpd_TaskIdThreadMap tpd
  ttm' = Map.insert tid thd ttm


getNextQueuedTask :: TaskProcessorData -> (Maybe TaskData, TaskProcessorData)
getNextQueuedTask tpd = (td , tpd { tpd_TaskQueue = q' })
  where
  q = tpd_TaskQueue tpd
  q' = if null q then q else tail q
  td = if null q then Nothing else Just $ head q



-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------


doProcessing :: TaskProcessor -> IO ()
doProcessing tp
  = do
    E.catchDyn (doProcessing' tp)
      handler
    where
      handler :: TaskException -> IO ()
      handler err = putStrLn (show err)
      doProcessing' tp'
        = do
          handleNewTasks tp'
          handleFinishedTasks tp'
          delay <- withMVar tp' (\tpd -> return $ tpd_ServerDelay tpd)
          threadDelay delay
          doProcessing' tp'


handleNewTasks :: TaskProcessor -> IO ()
handleNewTasks tp
  = do
    modifyMVar tp $
      \tpd ->
      do
      tpd' <- handleNewTasks' tpd
      return (tpd',())
    where
    handleNewTasks' tpd 
      = do
        -- we can only start new tasks, if there are any left...
        let maxTasks = tpd_MaxTasks tpd
        let runningTasks = Map.size (tpd_TaskIdThreadMap tpd)
        let moreTasks = not $ null (tpd_TaskQueue tpd)
        if (moreTasks && (runningTasks < maxTasks)) 
          then do
            -- take the task from the queue
            let (mtd, tpd') = getNextQueuedTask tpd
            let td = fromJust mtd
            -- start its thread
            thd <- runTask td tp
            -- save it
            let tpd'' = setTaskRunning (td_TaskId td) thd tpd'
            -- try to start more
            handleNewTasks' tpd''
          else do
            return tpd


runTask :: TaskData -> TaskProcessor -> IO ThreadId
runTask td tp
  = do
    -- spawn a new thread for each tasks
    forkIO $ 
      E.handle (\_ -> reportErrorTask td tp) $ 
        do
        yield
        td' <- case (td_Type td) of
          TTMap     -> performMapTask td tp
          TTCombine -> performCombineTask td tp 
          TTReduce  -> performReduceTask td tp
          _         -> E.throwDyn UnkownTaskException
        reportCompletedTask td' tp
      
    
handleFinishedTasks :: TaskProcessor -> IO ()
handleFinishedTasks tp
  = do
    modifyMVar tp $
      \tpd ->
      do
      cts' <- sendTasksResults (tpd_CompletedTasks tpd) (tpf_TaskCompleted $ tpd_Functions tpd)
      ets' <- sendTasksResults (tpd_ErrorTasks tpd) (tpf_TaskError $ tpd_Functions tpd)
      let tpd' = tpd { tpd_CompletedTasks = cts', tpd_ErrorTasks = ets' }
      return (tpd', ()) 

      
sendTasksResults :: Set.Set TaskData -> TaskResultFunction -> IO (Set.Set TaskData)
sendTasksResults set fun
  = do
    let ls = Set.toList set
    sendResults <- mapM fun ls 
    let (failures,_) = unzip $ filter (\(_,b) -> not b) $ zip ls sendResults
    return $ Set.fromList failures





-- ----------------------------------------------------------------------------
-- Contruction and Initialisation
-- ----------------------------------------------------------------------------


-- | doing a map task
performMapTask :: TaskData -> TaskProcessor-> IO TaskData
performMapTask td _
  = do
    putStrLn "MapTask"
    putStrLn $ show td 
    return td
    -- content <- F.getFileContent fid fs
    -- let input = fileReader fid content
    -- let fct = dispatchMapFunction myFunctions a
    -- let outputs = map (\(k,v) -> fct k v) input
    -- results <- sequence outputs
    -- putStrLn $ show results
    -- return fid




performCombineTask :: TaskData -> TaskProcessor-> IO TaskData
performCombineTask td _
  = do
    putStrLn "CombineTask"
    putStrLn $ show td 
    return td
  

performReduceTask :: TaskData -> TaskProcessor-> IO TaskData
performReduceTask td _
  = do
    putStrLn "ReduceTask"
    putStrLn $ show td 
    return td





-- ----------------------------------------------------------------------------
-- Contruction and Initialisation
-- ----------------------------------------------------------------------------
{-



-}


-- ----------------------------------------------------------------------------
-- Performing Tasks
-- ----------------------------------------------------------------------------

{-
    

-}

-- ----------------------------------------------------------------------------
-- GroupByKey
-- ----------------------------------------------------------------------------

-- groupByKey :: (Ord k2) => [(k2, v2)] -> [(k2,[v2])]
-- groupByKey ls = Map.toList $ foldl insert Map.empty ls
--  where
--    insert dict (k2,v2) = Map.insertWith (++) k2 [v2] dict


-- ----------------------------------------------------------------------------
-- FileReader
-- ----------------------------------------------------------------------------

-- fileReader :: S.FileId -> Maybe S.FileContent -> [(B.ByteString, B.ByteString)]
-- fileReader _ Nothing = []
-- fileReader f (Just (TextFile c)) = [(encode f, encode c)]
-- fileReader f (Just (BinaryFile c)) = [(encode f, c)]     

