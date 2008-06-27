
module Holumbus.Master.ProcessScheduler
(
  ProcessScheduler
, emptyProcessScheduler
, startScheduler
, stopScheduler
, addWorker
, deleteWorker
, addTask
, deleteTask

, updateWorkerLifeTime
, getWorkers
)
where

import Control.Concurrent

import qualified Holumbus.Master.WorkerRegistry as WR
import qualified Holumbus.Master.ProcessRegistry as PR
import qualified Holumbus.Task.TaskRegistry as TR
import qualified Holumbus.Task.TaskData as TD

data ProcessScheduler = MkProcessScheduler 
  {
    schedulerThreadId :: Maybe ThreadId   
  , procReg :: PR.ProcessRegistry
  , workReg :: WR.WorkerRegistry
  , taskReg :: TR.TaskRegistry
  } deriving (Show)
  
emptyProcessScheduler :: ProcessScheduler
emptyProcessScheduler 
  = MkProcessScheduler
    Nothing
    PR.emptyProcessRegistry
    WR.emptyWorkerRegistry
    TR.emptyTaskRegistry

startScheduler :: MVar ProcessScheduler -> IO (Bool)
startScheduler dat
  = do
    sched <- takeMVar dat
    (r, newThreadId) <- case (schedulerThreadId sched) of
      Nothing -> do 
                 putStrLn "starting scheduler"
                 i <- forkIO $ doScheduling dat
                 return (True, Just i)
      Just i  -> do 
                 putStrLn "scheduler already started"
                 return (False, Just i)
    putMVar dat (sched { schedulerThreadId = newThreadId })
    return r

stopScheduler :: MVar ProcessScheduler -> IO (Bool)
stopScheduler dat
  = do
    sched <- takeMVar dat
    (r, newThreadId) <- case (schedulerThreadId sched) of
      Nothing -> do 
                 putStrLn "no scheduler running"
                 return (False, Nothing)
      Just i  -> do 
                 putStrLn "stopping scheduler"
                 killThread i
                 return (True, Nothing)
    putMVar dat (sched { schedulerThreadId = newThreadId })
    return r

doScheduling :: MVar ProcessScheduler -> IO ()
doScheduling psMVar
  = do
    threadDelay 10000000 -- 10 sec
    putStrLn "startScheduling..."
    
    doScheduling psMVar

--ext:
    
addWorker :: WR.WorkerData -> ProcessScheduler -> (WR.WorkerId, ProcessScheduler)
addWorker wd ps 
  = (idx, ps { workReg = newWorkReg } )
  where
    (idx, newWorkReg) = WR.registerWorker wd (workReg ps)

deleteWorker :: WR.WorkerId -> ProcessScheduler -> ProcessScheduler
deleteWorker idx ps
  = ps { workReg = newWorkReg }
  where
    newWorkReg = WR.unregisterWorker idx (workReg ps)
-- TODO Inform workers about abortion... or set Task Status to deleted...

addTask :: TD.TaskData -> ProcessScheduler -> (TD.TaskId, ProcessScheduler)
addTask td ps
  = (idx, ps { taskReg = newTaskReg } )
  where
    (idx, newTaskReg) = TR.insertTask td (taskReg ps)
   
deleteTask :: TD.TaskId -> ProcessScheduler -> ProcessScheduler
deleteTask idx ps
  = ps { taskReg = newTaskReg }
  where
    newTaskReg = TR.deleteTask idx (taskReg ps)
-- TODO Inform workers about abortion... or set Task Status to deleted...
    
  
updateWorkerLifeTime :: [(WR.WorkerId, Bool)] -> ProcessScheduler -> ProcessScheduler
updateWorkerLifeTime lst ps
  = ps { workReg = newWorkReg }
  where
    newWorkReg = WR.updateWorkerLifeTime lst (workReg ps)

getWorkers :: ProcessScheduler -> [WR.WorkerData]
getWorkers ps = WR.getWorkers (workReg ps)

--setTaskFinished

--int:

--startTask

--stopTask