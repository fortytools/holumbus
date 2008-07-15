-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.MapReduce.Demo
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.MapReduce.Demo
(
  demoMapFunctions
, demoReduceFunctions
, demoPartitionFunctions
, demoJob
, createDemoFiles
)
where

import Data.Binary
import qualified Data.ByteString.Lazy as B

import qualified Holumbus.FileSystem.FileSystem as FS
import qualified Holumbus.FileSystem.Storage as S

import Holumbus.MapReduce.Types
import Holumbus.MapReduce.JobController
import qualified Holumbus.MapReduce.AccuMap as AMap

-- ----------------------------------------------------------------------------
-- MapFunctions
-- ----------------------------------------------------------------------------


mapId :: B.ByteString -> B.ByteString -> IO [(B.ByteString, B.ByteString)]
mapId k v
  = do
    return [(k, v)]


mapWordCount :: String -> String -> IO [(String, Integer)]
mapWordCount k v
  = do 
    putStrLn "mapCountWords"
    putStrLn $ show ("input: " ++ k ++ " - " ++ show v)
    let v' = map (\s -> (s,1)) $ words v
    putStrLn $ show $ "output: " ++ show v'
    return v'


demoMapFunctions :: MapFunctionMap
demoMapFunctions 
  = addMapFunctionToMap mapWordCount "WORDCOUNT" "counts the words in a text" $ 
    addMapFunctionToMap mapId "ID" "does nothing" $
    emptyMapFunctionMap



-- ----------------------------------------------------------------------------
-- ReduceFunctions
-- ----------------------------------------------------------------------------

reduceId :: B.ByteString -> [B.ByteString] -> IO (Maybe [B.ByteString])
reduceId _ vs = return (Just vs)

reduceWordCount :: String -> [Integer] -> IO (Maybe Integer)
reduceWordCount k vs 
  = do
    putStrLn "reduce/combine CountWords"
    putStrLn $ show ("input: " ++ k ++ " - " ++ show vs)
    let s = sum vs
    putStrLn $ show $ "output: " ++ show s
    return (Just s)
    
demoReduceFunctions :: ReduceFunctionMap
demoReduceFunctions 
  = addReduceFunctionToMap reduceWordCount "WORDCOUNT" "counts the words in a text" $
    addReduceFunctionToMap reduceId "ID" "does nothing" $
    emptyReduceFunctionMap
  

-- ----------------------------------------------------------------------------
-- PartitionFunctions
-- ----------------------------------------------------------------------------  

partitionWordCount :: [(String, [Integer])] -> IO [(String, [Integer])]
partitionWordCount ls 
  = do
    let ls' = AMap.toList $ AMap.fromList ls
    return ls' 


demoPartitionFunctions :: PartitionFunctionMap
demoPartitionFunctions
  = addPartitionFunctionToMap partitionWordCount "WORDCOUNT" "counts the words in a text" $
    emptyPartitionFunctionMap

  
-- ----------------------------------------------------------------------------
-- DemoJob
-- ----------------------------------------------------------------------------

  
demoJob :: JobInfo
demoJob = JobInfo 
  "demo-WordcountJob"
  (Just "WORDCOUNT")
  (Just "WORDCOUNT")
  Nothing
  Nothing
  (Just "WORDCOUNT")
  Nothing
  (encodeTupleList [("text1", "aaa bb c dd dd"),("text2", "aaa bb"),("text2", "aaa")])




-- ----------------------------------------------------------------------------
-- DemoFiles
-- ----------------------------------------------------------------------------


createDemoFiles :: FS.FileSystem -> IO ()
createDemoFiles fs
  = do
    let c = S.BinaryFile (encode "a aa aaa b bb bbb")
    FS.createFile "foo" c fs