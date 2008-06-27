
module Holumbus.Task.TaskData
(
  TaskId
, TaskData
  
, mkNewTask
, validId
, getTaskId
, setTaskId
, getProgramName
, getParameters
)
where

import Data.Binary

type TaskId   =   Integer
data TaskData = TaskData
                ( TaskId   -- ^ TaskId
                , String   -- ^ Program name
                , [String] -- ^ Program Parameters
                ) deriving (Show)

instance Binary TaskData where
  put (TaskData (i, pn, pa)) = put i >> put pn >> put pa
  get = do
    i <- get
    pn <- get
    pa <- get
    return (TaskData (i, pn, pa))
    
mkNewTask :: String -> [String] -> TaskData
mkNewTask pn pa = TaskData ( -1, pn, pa )

validId :: TaskId -> Bool
validId = (>0)

getTaskId :: TaskData -> TaskId
getTaskId (TaskData (i, _, _)) = i

setTaskId :: TaskId -> TaskData -> TaskData
setTaskId i (TaskData (_, pn, pa)) = TaskData (i, pn, pa)

getProgramName :: TaskData -> String
getProgramName (TaskData (_, pn, _)) = pn

getParameters :: TaskData -> [String]
getParameters (TaskData (_, _, pa)) = pa