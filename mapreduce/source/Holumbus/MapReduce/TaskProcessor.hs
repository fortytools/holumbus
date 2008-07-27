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

, printTaskProcessor

-- * Creation and Destruction
, newTaskProcessor
, closeTaskProcessor
, setFileSystemToTaskProcessor
, setMapActionMap
, setReduceActionMap
, setTaskCompletedHook  
, setTaskErrorHook

-- * TaskProcessor 
, startTaskProcessor
, stopTaskProcessor

-- * Info an Debug
, listTaskIds 
, getMapActions
, getReduceActions


-- * Task Creation and Destruction
, startTask
, stopTask
, stopAllTasks 
)
where

import qualified Control.Exception as E
import           Control.Concurrent
import           Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Maybe
import           Data.Typeable
import           System.Log.Logger

import           Holumbus.MapReduce.Types
import qualified Holumbus.FileSystem.FileSystem as FS



localLogger :: String
localLogger = "Holumbus.MapReduce.TaskProcessor"


taskLogger :: String
taskLogger = "Holumbus.MapReduce.TaskProcessor.task"

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


data TaskProcessorException 
  = KillServerException
  deriving (Show, Typeable)

data TaskException
   = KillTaskException
   | UnkownTaskException
   deriving (Show, Typeable)


-- | data, needed by the MapReduce-System to run the tasks
data TaskProcessorData = TaskProcessorData {
  -- internal
    tpd_ServerThreadId    :: Maybe ThreadId
  , tpd_ServerDelay       :: Int
  , tpd_FileSystem        :: Maybe FS.FileSystem
  -- configuration
  , tpd_MaxTasks          :: Int
  , tpd_Functions         :: TaskProcessorFunctions
  , tpd_MapActionMap      :: MapActionMap
  , tpd_ReduceActionMap   :: ReduceActionMap
  -- task processing
  , tpd_TaskQueue         :: [TaskData]
  , tpd_CompletedTasks    :: Set.Set TaskData
  , tpd_ErrorTasks        :: Set.Set TaskData
  , tpd_TaskIdThreadMap   :: Map.Map TaskId ThreadId
  } deriving (Show)


type TaskProcessor = MVar TaskProcessorData


printTaskProcessor :: TaskProcessor -> IO String
printTaskProcessor tp
  = withMVar tp $ \tpd -> return $ show tpd



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
      1000 -- one millisecond delay
      Nothing
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
    -- do not start this, perhaps the user want to change something first
    -- startTaskProcessor tp
    return tp


closeTaskProcessor :: TaskProcessor -> IO ()
closeTaskProcessor tp
  = do
    stopTaskProcessor tp


-- | add a filesystem-instance to the TaskProcessor
setFileSystemToTaskProcessor :: FS.FileSystem -> TaskProcessor -> IO ()
setFileSystemToTaskProcessor fs tp
  = modifyMVar tp $
    \tpd -> return $ (tpd { tpd_FileSystem = Just fs }, ())
  

-- | adds a MapAction to the TaskProcessor
setMapActionMap :: MapActionMap -> TaskProcessor -> IO ()
setMapActionMap m tp
  = modifyMVar tp $
    \tpd -> return $ (tpd { tpd_MapActionMap = m }, ())


-- | adds a ReduceAction to the TaskProcessor
setReduceActionMap :: ReduceActionMap -> TaskProcessor -> IO ()
setReduceActionMap m tp
  = modifyMVar tp $
      \tpd -> return $ (tpd { tpd_ReduceActionMap = m }, ())


setTaskCompletedHook :: TaskResultFunction -> TaskProcessor -> IO ()  
setTaskCompletedHook f tp
  = modifyMVar tp $
      \tpd ->
      do
      let funs = tpd_Functions tpd
      let funs' = funs { tpf_TaskCompleted = f }
      return (tpd { tpd_Functions = funs' }, ())


setTaskErrorHook :: TaskResultFunction -> TaskProcessor -> IO ()
setTaskErrorHook f tp
  = modifyMVar tp $
      \tpd ->
      do
      let funs = tpd_Functions tpd
      let funs' = funs { tpf_TaskError = f }
      return (tpd { tpd_Functions = funs' }, ())

-- ----------------------------------------------------------------------------
-- server functions
-- ----------------------------------------------------------------------------

startTaskProcessor :: TaskProcessor -> IO ()
startTaskProcessor tp
  = do
    modifyMVar tp $ 
      \tpd -> 
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
      \tpd -> 
      do
      case (tpd_ServerThreadId tpd) of
        (Nothing) -> return ()
        (Just i) -> 
          do
          E.throwDynTo i KillServerException
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


-- | Lists all Map-Actions with Name, Descrition and Type
getMapActions :: TaskProcessor -> IO [MapActionData]
getMapActions tp
  = withMVar tp $
      \tpd -> return $ Map.elems (tpd_MapActionMap tpd)


-- | Lists all Reduce-Actions with Name, Descrition and Type
getReduceActions :: TaskProcessor -> IO [ReduceActionData]
getReduceActions tp 
  = withMVar tp $
      \tpd -> return $ Map.elems (tpd_ReduceActionMap tpd) 


-- ----------------------------------------------------------------------------
-- Task Controlling
-- ----------------------------------------------------------------------------


-- | adds a Task to the TaskProcessor, the execution might be later
startTask :: TaskData -> TaskProcessor -> IO ()
startTask td tp
  = do
    debugM localLogger $ "waiting to add Task " ++ show (td_TaskId td)
    modifyMVar tp $
      \tpd->
      do
      debugM localLogger $ "adding Task " ++ show (td_TaskId td)
      return (addTask td tpd, ())
      
  
stopTask :: TaskId -> TaskProcessor -> IO ()
stopTask tid tp
  = do
    debugM localLogger $ "waiting to stop Task " ++ show tid
    mthd <- modifyMVar tp $
      \tpd-> 
      do
      let thd = getTaskThreadId tid tpd 
      return (deleteTask tid tpd, thd)
    debugM localLogger $ "stopping Task " ++ show tid
    maybe (return ()) (\thd -> E.throwDynTo thd KillTaskException) mthd


stopAllTasks :: TaskProcessor -> IO () 
stopAllTasks tp
  = do
    debugM localLogger $ "waiting to stop all Tasks"
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
      -- let f = tpf_TaskError $ tpd_Functions tpd'
      -- f td
      return (tpd', ())


-- | mark the task as completed and invoke the reply function
reportCompletedTask :: TaskData -> TaskProcessor -> IO ()
reportCompletedTask td tp
  = modifyMVar tp $
      \tpd -> 
      do 
      let tpd' = setTaskCompleted td tpd
      -- let f = tpf_TaskCompleted $ tpd_Functions tpd'
      -- f td
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
      handler :: TaskProcessorException -> IO ()
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
        -- threadDelay 5000000
        td' <- case (td_Type td) of
          TTMap     -> performMapTask td tp
          TTCombine -> performCombineTask td tp 
          TTReduce  -> performReduceTask td tp
          _         -> E.throwDyn UnkownTaskException
        reportCompletedTask td' tp
      
-- not used, because we are doi    
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
performMapTask td tp
  = do
    infoM taskLogger $ "MapTask " ++ show (td_TaskId td)
    debugM taskLogger $ "input td: " ++ show td
    
    -- get all functions
    (ad, bin, mbfs, tot) <- withMVar tp $
      \tpd ->
      do
      let action     = Map.lookup (ta_Action $ td_Action td) (tpd_MapActionMap tpd)
      let input      = (td_Input td)
      let filesystem = (tpd_FileSystem tpd)
      let tot = ta_OutputType $ td_Action td
      return (action, input, filesystem, tot)
    
    case ad of
      (Nothing) ->
        -- TODO throw execption here
        return td
      (Just a)  ->
        do
        let action = mad_Action a
        bin' <- loadInputList td mbfs bin
        bout <- action 1 bin'
        bout' <- saveOutputList td tot mbfs bout
        let td' = td { td_Output = bout' }
        debugM taskLogger $ "output td: " ++ show td'
        return td'
      

performCombineTask :: TaskData -> TaskProcessor-> IO TaskData
performCombineTask td tp
  = do
    infoM taskLogger $ "CombineTask " ++ show (td_TaskId td)
    debugM taskLogger $ "input td: " ++ show td
    
    -- get all functions
    (ad, bin, mbfs, tot) <- withMVar tp $
      \tpd ->
      do
      let action     = Map.lookup (ta_Action $ td_Action td) (tpd_ReduceActionMap tpd)
      let input      = (td_Input td)
      let filesystem = (tpd_FileSystem tpd)
      let tot = ta_OutputType $ td_Action td
      return (action, input, filesystem, tot)
    
    case ad of
      (Nothing) ->
        -- TODO throw execption here
        return td
      (Just a)  ->
        do
        let action = rad_Action a
        bin' <- loadInputList td mbfs bin
        bout <- action 1 bin'
        bout' <- saveOutputList td tot mbfs bout
        let td' = td { td_Output = bout' }
        debugM taskLogger $ "output td: " ++ show td'
        return td'
          

performReduceTask :: TaskData -> TaskProcessor-> IO TaskData
performReduceTask td tp
  = do
    infoM taskLogger $ "ReduceTask " ++ show (td_TaskId td)
    debugM taskLogger $ "input td: " ++ show td
    
    
    -- get all functions
    (ad, bin, mbfs, tot) <- withMVar tp $
      \tpd ->
      do
      let action     = Map.lookup (ta_Action $ td_Action td) (tpd_ReduceActionMap tpd)
      let input      = (td_Input td)
      let filesystem = (tpd_FileSystem tpd)
      let tot = ta_OutputType $ td_Action td
      return (action, input, filesystem, tot)
    
    case ad of
      (Nothing) ->
        -- TODO throw execption here
        return td
      (Just a)  ->
        do
        let action = rad_Action a
        bin' <- loadInputList td mbfs bin
        bout <- action 1 bin'
        bout' <- saveOutputList td tot mbfs bout
        let td' = td { td_Output = bout' }
        debugM taskLogger $ "output td: " ++ show td'
        return td'




loadInputList :: TaskData -> Maybe FS.FileSystem -> [FunctionData] -> IO [B.ByteString]
loadInputList _ mbfs is
  = do
    os <- mapM (loadInput mbfs) is
    let os' = catMaybes os
    return os'
    where
    loadInput _         (RawFunctionData b)  = return $ Just b
    -- TODO throw exception here
    loadInput Nothing   (FileFunctionData _) = return Nothing
    loadInput (Just fs) (FileFunctionData f)
      = do
        mbc <- FS.getFileContent f fs
        if isNothing mbc 
          then do
            return Nothing
          else do
            let c = fromJust mbc
            d <- case c of
              (FS.TextFile s)  -> return $ encode s
              (FS.BinaryFile b) -> return b
            return $ Just d

saveOutputList :: TaskData -> TaskOutputType -> Maybe FS.FileSystem -> [(Int, [B.ByteString])] -> IO [(Int,[FunctionData])]
saveOutputList _ _ mbfs os
  = do
    mapM (saveOutput mbfs) os
    where
    saveOutput Nothing   (i,bs) = return (i, map (\b -> RawFunctionData b) bs)
    saveOutput (Just fs) (i,bs) = return (i, map (\b -> RawFunctionData b) bs)


-- ----------------------------------------------------------------------------
-- FileReader
-- ----------------------------------------------------------------------------

-- fileReader :: S.FileId -> Maybe S.FileContent -> [(B.ByteString, B.ByteString)]
-- fileReader _ Nothing = []
-- fileReader f (Just (TextFile c)) = [(encode f, encode c)]
-- fileReader f (Just (BinaryFile c)) = [(encode f, c)]     

