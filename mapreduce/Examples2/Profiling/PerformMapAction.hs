
module Main(main) where

import qualified Data.ByteString.Lazy as B
import           Data.Binary
import           System.IO
import           System.Environment
import qualified Data.HashTable as Hash

import qualified Holumbus.Data.AccuMap as AMap
import           Holumbus.MapReduce.Types

dummyMapFunction :: ActionEnvironment -> () -> Int -> Int -> IO [(Int,Int)]
dummyMapFunction _ _ k v = return $ [(k,v)]

-- dummyReduceFunction :: ActionEnvironment -> () -> Int -> [Int] -> IO (Maybe Int)
-- dummyReduceFunction _ _ k _ = return $ Just k

getParams :: IO [String]
getParams
    = do
      args <- getArgs
      return (args ++ repeat "")

{-
myAction
  :: ActionConfiguration 
       ()                                          -- state
       Int Int                                     -- k1, v1
       Int Int                                     -- k2, v2
       Int                                         -- v3 == v2
       Int                                         -- v4
myAction
  = (defaultActionConfiguration "WORDFREQUENCY")
        { ac_Map     = Just mapAction
        , ac_Combine = Nothing
        , ac_Reduce  = Just reduceAction
        }
    where
      mapAction 
        = (defaultMapConfiguration dummyMapFunction)
      reduceAction
        = (defaultReduceConfiguration dummyReduceFunction)
-}

dummyInput :: Int -> Int -> (Int, [FunctionData])
dummyInput n p = (p,dummyInput' n)
  where
  dummyInput' 0  = []
  dummyInput' n' = (TupleFunctionData (encode ((n `mod` p) + 1 ,n))) : dummyInput' (n' - 1)

dummyTaskData :: TaskData
dummyTaskData
  = TaskData
      1
      1
      TTMap
      TSCompleted
      (encode ())
      (Just 5)
      (3,[])
      []
      TOTRawTuple
      "myAction"

performMapAction
  :: (Ord k2,
      Binary a, Binary k1, Binary v1, Binary k2, Binary v2)
  => OptionsDecoder a
  -> MapFunction a k1 v1 k2 v2
  -> MapPartition a k2 v2
  -> InputReader k1 v1
  -> OutputWriter k2 v2
  -> ActionEnvironment
  -> B.ByteString
  -> Maybe Int
  -> (Int,[FunctionData])
  -> IO [(Int, [FunctionData])]
performMapAction optDec fct part reader writer env opts n (i,ls)
  = do
    -- decode the options
    let a = optDec opts
    
    putStrLn "performMapAction"
    
    putStrLn "reading inputList"
    inputList <- {-# SCC "readConnector" #-} readConnector reader env ls
    
    putStrLn "doing map"
    mappedList <- {-# SCC "mapping" #-} mapM (\(k1, v1) -> fct env a k1 v1) inputList
    let tupleList = concat mappedList
     
    putStrLn "doing partition"
    partedList <- {-# SCC "partition" #-} case n of
      (Just n') -> part env a n' tupleList
      (Nothing) -> return [(i,tupleList)]
    
    putStrLn "writing outputlist"
    outputList <- {-# SCC "writeConnector" #-} writeConnector writer env partedList
    return outputList


myPartition
  :: (Binary k2, Binary v2)
  => MapPartition a k2 v2
myPartition _ _ 1 ls
  -- To make it faster, wenn no partition is used
  = return [(1,ls)]
myPartition _ _ n ls
  = do
    -- calculate partition-Values
    let markedList = map (\t@(k,_) -> (hash k,t)) ls
    -- merge them
    
    -- TODO this might change (revert) the order of the Elements...
    let resultList = AMap.toList $ AMap.fromTupleList markedList
    return resultList
    where
    -- calculate a hash-value, because we only have the Binary-Instance, we
    -- can only use the Bytestring of the Value
    hash k = ((fromIntegral $ Hash.hashString (show $ encode k)) `mod` n) + 1


main :: IO ()
main
  = do
    args <-getParams
    
    let td    = dummyTaskData
        fs    = error "no fs"
        opt   = encode ()
        parts = Just 5
        n     = read $ head args
        bin   = dummyInput n 5
         
    -- let ad = readActionConfiguration myAction
    -- action <- case (getActionForTaskType TTMap ad) of
    --   (Just a') -> return a'
    --   (Nothing) -> error "no action found"
    let env = mkActionEnvironment td fs
    
    bout <- performMapAction 
             (decode)
             dummyMapFunction
             myPartition
             defaultInputReader
             defaultOutputWriter
             env opt parts bin
    -- bout <-  action env opt parts bin
    putStrLn $ show (length bout)
     
    return ()
