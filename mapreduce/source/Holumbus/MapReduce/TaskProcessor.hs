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

module Holumbus.MapReduce.TaskProcessor
(
-- * Datatypes
  TaskProcessorData

-- * Contruction and Initialisation
, newTaskProcessor
, setMapFunctionMap
, setReduceFunctionMap

-- * Info and Debug
, getMapFunctions
, getReduceFunctions

-- * PerformTasks
, performTask
)
where

import Data.Binary
import qualified Data.Map as Map
import Data.Typeable

import Holumbus.MapReduce.Types
import Holumbus.MapReduce.JobController

import Holumbus.FileSystem.FileSystem as F
import Holumbus.FileSystem.Storage as S





-- ----------------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------------


-- | data, needed by the MapReduce-System to run the tasks
data TaskProcessorData = TaskProcessorData {
    mr_fileSystem        :: F.FileSystem
  , mr_MapFunctionMap    :: MapFunctionMap
  , mr_ReduceFunctionMap :: ReduceFunctionMap
  }


-- ----------------------------------------------------------------------------
-- Contruction and Initialisation
-- ----------------------------------------------------------------------------


-- | creates a new TaskProcessor
newTaskProcessor :: F.FileSystem -> TaskProcessorData
newTaskProcessor fs = TaskProcessorData fs emptyMapFunctionMap emptyReduceFunctionMap


-- | adds a MapFunction to the TaskProcessor
setMapFunctionMap :: MapFunctionMap -> TaskProcessorData -> TaskProcessorData
setMapFunctionMap m tpd = tpd { mr_MapFunctionMap = m }


-- | adds a ReduceFunction to the TaskProcessor
setReduceFunctionMap :: ReduceFunctionMap -> TaskProcessorData -> TaskProcessorData
setReduceFunctionMap m tpd = tpd { mr_ReduceFunctionMap = m }




-- ----------------------------------------------------------------------------
-- Info an Debug
-- ----------------------------------------------------------------------------


-- | Lists all Map-Functions with Name, Descrition and Type
getMapFunctions :: TaskProcessorData -> [(FunctionName, FunctionDescription, TypeRep)]
getMapFunctions (TaskProcessorData _ m _) = map (\(n,d,t,_)->(n,d,t)) (Map.elems m)


-- | Lists all Reduce-Functions with Name, Descrition and Type
getReduceFunctions :: TaskProcessorData -> [(FunctionName, FunctionDescription, TypeRep)]
getReduceFunctions (TaskProcessorData _ _ m) = map (\(n,d,t,_)->(n,d,t)) (Map.elems m)




-- ----------------------------------------------------------------------------
-- Performing Tasks
-- ----------------------------------------------------------------------------


-- | dispatcher for the TaskType
performTask :: TaskProcessorData -> TaskData -> IO TaskData
performTask mrd td
  = case (td_Type td) of
      TTMap     -> performMapTask mrd td
      TTCombine -> performCombineTask mrd td 
      TTReduce  -> performReduceTask mrd td
      _         -> return td
    

-- | doing a map task
performMapTask :: TaskProcessorData -> TaskData -> IO TaskData
performMapTask _ td
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




performCombineTask ::TaskProcessorData -> TaskData -> IO TaskData
performCombineTask _ td
  = do
    putStrLn "CombineTask"
    putStrLn $ show td 
    return td
  

performReduceTask :: TaskProcessorData -> TaskData -> IO TaskData
performReduceTask _ td
  = do
    putStrLn "ReduceTask"
    putStrLn $ show td 
    return td


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

