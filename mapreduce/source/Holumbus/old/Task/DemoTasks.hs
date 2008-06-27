
module Holumbus.Task.DemoTasks 
(
    demoTaskLookUpMap
  , demoTaskNameSet
)
where

import qualified Data.Map as M

import Holumbus.Control.MapReduce.Parallel

import Holumbus.Task.TaskProcessor

demoTaskNameSet :: TaskNameSet
demoTaskNameSet = getTaskNameSet $ demoTaskLookUpMap

demoTaskLookUpMap :: TaskLookUpMap
demoTaskLookUpMap
  = addTask "WORDCOUNT" wordCountFct $
    emptyTaskLookUpMap


wordCountFct :: IO ()
wordCountFct
  = do
    putStrLn "calculating word count..."
    r <- mapReduce 1 mapWordCount reduceWordCount inputWordFct
    outputWordFct r    

mapWordCount :: (Int -> String -> IO [(String, Int)])
mapWordCount _ v 
  = do
    return (zip (words v) ones)
    where
      ones = 1 : ones

reduceWordCount :: (String -> [Int] -> IO (Maybe Int))
reduceWordCount _ v = return (Just (sum v))

inputWordFct :: [(Int, String)]
inputWordFct = [(1, "hallo welt"), (2, "hallo john"), (3, "hallo john")]

outputWordFct :: M.Map String Int -> IO ()
outputWordFct m
  = do
    putStrLn (show m)