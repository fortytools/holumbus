
module Holumbus.Master.ProcessRegistry
(
  ProcessRegistry
  
, emptyProcessRegistry
, registerProcess
, unregisterTask
, unregisterWorker
, getTasksFromWorker
, getWorkersFromTask
)
where

import qualified Data.Map as M
import qualified Data.Set as S

import Holumbus.Master.WorkerRegistry (WorkerId)
import Holumbus.Task.TaskData (TaskId)

data ProcessRegistry = ProcessRegistry 
                       (  
                         M.Map WorkerId (S.Set TaskId)
                       , M.Map TaskId (S.Set WorkerId)
                       ) deriving (Show)
                       
emptyProcessRegistry :: ProcessRegistry
emptyProcessRegistry = ProcessRegistry ( M.empty, M.empty )

registerProcess :: TaskId -> WorkerId -> ProcessRegistry -> ProcessRegistry
registerProcess t w (ProcessRegistry (mwt, mtw))
  = ProcessRegistry ( M.insert w ts mwt, M.insert t ws mtw )
  where
    ts = maybe ( S.empty ) ( S.insert t ) ( M.lookup w mwt )
    ws = maybe ( S.empty ) ( S.insert w ) ( M.lookup t mtw )
  
unregisterTask :: TaskId -> ProcessRegistry -> ProcessRegistry
unregisterTask i r
  = ProcessRegistry ( newmwt, newmtw )
  where
    ProcessRegistry ( newmwt, _ ) = S.fold (\w reg -> deleteTaskFromWorker w i reg) r (getWorkersFromTask i r)
    ProcessRegistry ( _, newmtw ) = deleteOnlyTask i r

unregisterWorker :: WorkerId -> ProcessRegistry -> ProcessRegistry
unregisterWorker i r
  = ProcessRegistry ( newmwt, newmtw )
  where
    ProcessRegistry ( newmwt, _ ) = deleteOnlyWorker i r
    ProcessRegistry ( _, newmtw ) = S.fold (\t reg -> deleteWorkerFromTask t i reg) r (getTasksFromWorker i r)

getTasksFromWorker :: WorkerId -> ProcessRegistry -> S.Set TaskId
getTasksFromWorker w (ProcessRegistry (mwt, _))
  = maybe ( S.empty ) ( id ) ( M.lookup w mwt )

getWorkersFromTask :: TaskId -> ProcessRegistry -> S.Set WorkerId
getWorkersFromTask t (ProcessRegistry (_, mtw))
  = maybe ( S.empty ) ( id ) ( M.lookup t mtw )

deleteOnlyTask :: TaskId -> ProcessRegistry -> ProcessRegistry
deleteOnlyTask t (ProcessRegistry (mwt, mtw))
  = ProcessRegistry ( mwt, M.delete  t mtw )
  
deleteOnlyWorker :: WorkerId -> ProcessRegistry -> ProcessRegistry
deleteOnlyWorker w (ProcessRegistry (mwt, mtw))
  = ProcessRegistry ( M.delete w mwt, mtw )

deleteTaskFromWorker :: WorkerId -> TaskId -> ProcessRegistry -> ProcessRegistry
deleteTaskFromWorker w t r@(ProcessRegistry (mwt, mtw))
  = ProcessRegistry ( newmwt, mtw )
  where
    newmwt = M.insert w (S.delete t $ getTasksFromWorker w r) mwt

deleteWorkerFromTask :: TaskId -> WorkerId -> ProcessRegistry -> ProcessRegistry
deleteWorkerFromTask t w r@(ProcessRegistry (mwt, mtw))
  = ProcessRegistry ( mwt, newmtw )
  where
    newmtw = M.insert t (S.delete w $ getWorkersFromTask t r) mtw

