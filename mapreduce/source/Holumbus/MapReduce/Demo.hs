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


-- ----------------------------------------------------------------------------
-- MapFunctions
-- ----------------------------------------------------------------------------


mapId :: B.ByteString -> B.ByteString -> IO [(B.ByteString, B.ByteString)]
mapId k v
  = do
    return [(k, v)]


mapWordCount :: String -> String -> IO [(String, Integer)]
mapWordCount _ v
  = do
    putStrLn "mapCountWords"
    let v' = map (\s -> (s,1)) $ words v
    putStrLn $ show v'
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
reduceWordCount _ vs = return (Just $ sum vs)
    
demoReduceFunctions :: ReduceFunctionMap
demoReduceFunctions 
  = addReduceFunctionToMap reduceWordCount "WORDCOUNT" "counts the words in a text" $
    addReduceFunctionToMap reduceId "ID" "does nothing" $
    emptyReduceFunctionMap
  
  

  
-- ----------------------------------------------------------------------------
-- DemoJob
-- ----------------------------------------------------------------------------

  
demoJob :: JobInfo
demoJob = JobInfo "demo-WordcountJob" (Just "WORDCOUNT") Nothing Nothing [mkTupleData ("text1", "aaa bbb ccc")] []




-- ----------------------------------------------------------------------------
-- DemoFiles
-- ----------------------------------------------------------------------------


createDemoFiles :: FS.FileSystem -> IO ()
createDemoFiles fs
  = do
    let c = S.BinaryFile (encode "a aa aaa b bb bbb")
    FS.createFile "foo" c fs