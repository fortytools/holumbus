
module Holumbus.Task.TaskRegistry
(
  TaskRegistry
  
, emptyTaskRegistry
, insertTask  
, deleteTask
, lookupTask
)
where

import qualified Data.Map as Map

import Holumbus.Task.TaskData

type TaskRegistry = (
                      TaskId
                    , Map.Map TaskId TaskData
                    )
              
emptyTaskRegistry :: TaskRegistry
emptyTaskRegistry = ( 1, Map.empty )

nextKey :: TaskId -> TaskId
nextKey = (+1)
                    
insertTask :: TaskData -> TaskRegistry -> (TaskId, TaskRegistry)
insertTask d (n, m) 
  | (validId taskId)
    = (taskId, (newKey, Map.insert taskId d m))
  | otherwise
    = (n, (nextKey n , Map.insert n newTask m))
  where
    newTask = setTaskId n d
    taskId = getTaskId d
    newKey = (nextKey taskId) `max` n
    
deleteTask :: TaskId -> TaskRegistry -> TaskRegistry
deleteTask i (n, m) = (n, Map.delete i m)

lookupTask :: TaskId -> TaskRegistry -> Maybe TaskData
lookupTask i (_, m) = Map.lookup i m
