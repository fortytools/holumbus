
module Holumbus.Task.TaskProcessor where

import Control.Monad
import qualified Data.Set as S
import qualified Data.Map as M

import Holumbus.Task.TaskData

type TaskNameSet = S.Set String
data TaskLookUpMap = TaskLookUpMap ( M.Map String (IO ()) )

instance Show TaskLookUpMap where
  show (TaskLookUpMap (m)) = show (M.keys m)
  
instance Eq TaskLookUpMap where
  (==) (TaskLookUpMap (m1)) (TaskLookUpMap (m2))
    = (==) (M.keys m1) (M.keys m2)

emptyTaskLookUpMap :: TaskLookUpMap
emptyTaskLookUpMap = TaskLookUpMap ( M.empty )

addTask :: String -> (IO ()) -> TaskLookUpMap -> TaskLookUpMap
addTask s f (TaskLookUpMap m) = TaskLookUpMap ( M.insert s f m )

lookUpTask :: String -> TaskLookUpMap -> Maybe (IO ())
lookUpTask s (TaskLookUpMap m) = M.lookup s m

getTaskNameSet :: TaskLookUpMap -> TaskNameSet
getTaskNameSet (TaskLookUpMap (m)) = M.keysSet m


processTask :: TaskLookUpMap -> TaskData -> IO (Bool)
processTask m d
  = do
    putStrLn "processing task"
    program <- return (lookUpTask (getProgramName d) m)
    case (program) of
      (Nothing) ->
        do
        putStrLn "no program found"
        return False
      (Just p) ->
        do        
        putStrLn "executing program..."
        p
        return True



      
--type MapFunction k1 v1 k2 v2 = Ord k2 => (k1 ->  v1  -> IO [(k2, v2)])
--type ReduceFunction k2 v2 v3 = Ord k2 => (k2 -> [v2] -> IO (Maybe v3))
--type InputData k1 v1         = [(k1, v1)] 
--type ResultData k2 v3        = IO (M.Map k2 v3)      
      
--programDispatcher :: MRProgram a => String -> Maybe a
--programDispatcher s
--  | s == "wordCount" = Just (CountProg ( "wordCount" , mapWordCount, reduceWordCount, inputWordFct ))
--  | otherwise = Nothing

--mapFunctionDispatcher :: String -> Maybe (k1 ->  v1  -> IO [(k2, v2)])
--mapFunctionDispatcher s
--  | s == "wordcount" = Just mapWordCount
--  | otherwise = Nothing


--reduceFunctionDispatcher :: String -> Maybe (k2 -> [v2] -> IO (Maybe v3))
--reduceFunctionDispatcher s
--  | s == "wordcount" = Just reduceWordCount
--  | otherwise = Nothing

{-
class MRProgram a where
  programName :: a -> String
  mapFct :: a -> (Int -> String -> IO [(String, Int)]) --Ord k2 => a -> (k1 ->  v1  -> IO [(k2, v2)])
  reduceFct :: a -> (String -> [Int] -> IO (Maybe Int)) --Ord k2 => a -> (k2 -> [v2] -> IO (Maybe v3))
  inputFct :: a -> [(Int, String)] --a -> [(k1, v1)]

data CountProg = CountProg ( String
                 , (Int -> String -> IO [(String, Int)])
                 , (String -> [Int] -> IO (Maybe Int))
                 , [(Int, String)]
                 )
                 
instance MRProgram CountProg where
  programName (CountProg ( s, _, _, _)) = s
  mapFct      (CountProg ( _, m, _, _)) = m
  reduceFct   (CountProg ( _, _, r, _)) = r
  inputFct    (CountProg ( _, _, _, i)) = i        
-}
