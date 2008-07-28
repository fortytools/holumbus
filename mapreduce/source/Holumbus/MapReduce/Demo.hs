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
  demoMapActions
, demoReduceActions
, demoJob
, createDemoFiles
)
where

import           Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map

import           System.Log.Logger

import qualified Holumbus.FileSystem.FileSystem as FS
import qualified Holumbus.FileSystem.Storage as S

import qualified Holumbus.Data.AccuMap as AMap
import           Holumbus.MapReduce.Types


localLogger :: String
localLogger = "Holumbus.MapReduce.Demo"

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
    infoM localLogger "mapCountWords"
    debugM localLogger $ show ("input: " ++ k ++ " - " ++ show v)
    let v' = map (\s -> (s,1)) $ words v
    debugM localLogger $ show $ "output: " ++ show v'
    return v'



-- ----------------------------------------------------------------------------
-- ReduceFunctions
-- ----------------------------------------------------------------------------

reduceId :: B.ByteString -> [B.ByteString] -> IO (Maybe B.ByteString)
reduceId k _ = return $ Just k

reduceWordCount :: String -> [Integer] -> IO (Maybe Integer)
reduceWordCount k vs 
  = do
    infoM localLogger "reduce/combine CountWords"
    debugM localLogger $ show ("input: " ++ k ++ " - " ++ show vs)
    let s = sum vs
    debugM localLogger $ show $ "output: " ++ show s
    return (Just s)
    
  

-- ----------------------------------------------------------------------------
-- MergeFunctions
-- ----------------------------------------------------------------------------

mergeX :: [(B.ByteString, B.ByteString)] -> IO [(B.ByteString, [B.ByteString])]
mergeX ls = return $ mapGroupByKey ls

mergeWordCount :: [(String,Integer)] -> IO [(String,[Integer])]
mergeWordCount ls = return $ mapGroupByKey ls

mapGroupByKey :: (Ord k2) => [(k2, v2)] -> [(k2,[v2])]
mapGroupByKey ls = AMap.toList $ AMap.fromTupleList ls 



-- ----------------------------------------------------------------------------
-- PartitionFunctions
-- ----------------------------------------------------------------------------  

partitionId :: Int -> [(B.ByteString, B.ByteString)] -> IO [(Int,[(B.ByteString, B.ByteString)])]
partitionId n vs
  = return [(n, vs)]

partitionWordCount :: Int -> [(String, Integer)] -> IO [(Int,[(String, Integer)])]
partitionWordCount _ ls 
  = do
    infoM localLogger "partitionCountWords"
    debugM localLogger $ show ls
    -- calculate partition-Values
    let markedList = map (\t@(k,_) ->  (length k,t)) ls
    -- merge them
    let resultList = AMap.toList $ AMap.fromTupleList markedList
    return resultList


-- ----------------------------------------------------------------------------
-- Actions
-- ----------------------------------------------------------------------------


demoMapActions :: MapActionMap
demoMapActions 
  = Map.insert "WORDCOUNT" wordCount $
    Map.insert "ID" idFct $
    Map.empty
    where
    wordCount = mkMapAction "WORDCOUNT" "counts the words in a text" mapWordCount partitionWordCount
    idFct = mkMapAction "ID" "foo" mapId partitionId


demoReduceActions :: ReduceActionMap
demoReduceActions
  = Map.insert "WORDCOUNT" wordCount $
    Map.insert "ID" idFct $ 
    Map.empty
    where
    wordCount = mkReduceAction "WORDCOUNT" "counts the words in a text" mergeWordCount reduceWordCount partitionWordCount
    idFct = mkReduceAction "ID" "foo" mergeX reduceId partitionId



-- ----------------------------------------------------------------------------
-- DemoJob
-- ----------------------------------------------------------------------------

  
demoJob :: JobInfo
demoJob = JobInfo 
  "demo-WordcountJob"
  (Just $ TaskAction "WORDCOUNT" TOTRaw )
  (Just $ TaskAction "WORDCOUNT" TOTRaw )
  Nothing
  ([RawFunctionData (encodeTuple ("text1", "aaa bb c dd dd"))
   ,RawFunctionData (encodeTuple ("text2", "aaa bb"))
   ,RawFunctionData (encodeTuple ("text2", "aaa dd dd"))
   ,FileFunctionData "file1.txt"
   ])




-- ----------------------------------------------------------------------------
-- DemoFiles
-- ----------------------------------------------------------------------------


createDemoFiles :: FS.FileSystem -> IO ()
createDemoFiles fs
  = do
    -- let c = S.BinaryFile (encode ("foo","a aa aaa b bb bbb"))
    let c = S.TextFile "a aa aaa b bb bbb"
    FS.createFile "file1.txt" c fs