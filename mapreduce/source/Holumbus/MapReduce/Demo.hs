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
  demoActions
, demoJob
, createDemoFiles
)
where

import           Data.Binary

import           System.Log.Logger

import qualified Holumbus.FileSystem.FileSystem as FS

import qualified Holumbus.Data.AccuMap as AMap
import qualified Holumbus.Data.KeyMap as KMap
import           Holumbus.MapReduce.Types


localLogger :: String
localLogger = "Holumbus.MapReduce.Demo"


-- ----------------------------------------------------------------------------
-- Word-Frequency
-- ----------------------------------------------------------------------------


mapWordFrequency :: ActionEnvironment -> () -> String -> String -> IO [(String, Integer)]
mapWordFrequency _ _ k v
  = do 
    infoM localLogger "mapWordFrequency"
    debugM localLogger $ show ("input: " ++ k ++ " - " ++ show v)
    let v' = map (\s -> (s,1)) $ words v
    debugM localLogger $ show $ "output: " ++ show v'
    return v'


reduceWordFrequency :: ActionEnvironment -> () -> String -> [Integer] -> IO (Maybe Integer)
reduceWordFrequency _ _ k vs 
  = do
    infoM localLogger "reduce/combine WordFrequency"
    debugM localLogger $ show ("input: " ++ k ++ " - " ++ show vs)
    let s = sum vs
    debugM localLogger $ show $ "output: " ++ show s
    return (Just s)
    
  
partitionWordFrequency :: ActionEnvironment -> () -> Int -> [(String, Integer)] -> IO [(Int,[(String, Integer)])]
partitionWordFrequency _ _ _ ls 
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

wordFrequencyAction
  :: ActionConfiguration 
       ()                                          -- state
       String String                               -- k1, v1
       String Integer                              -- k2, v2
       Integer                                     -- v3 == v2
       Integer                                     -- v4
wordFrequencyAction
  = (defaultActionConfiguration "WORDFREQUENCY")
        { ac_Map     = Just mapAction
        , ac_Combine = Nothing
        , ac_Reduce  = Just reduceAction
        }
    where
      mapAction 
        = (defaultMapConfiguration mapWordFrequency)
            { mc_Partition = partitionWordFrequency }
      reduceAction
        = (defaultReduceConfiguration reduceWordFrequency)
            { rc_Partition = partitionWordFrequency }

demoActions :: ActionMap
demoActions
  = KMap.insert (readActionConfiguration wordFrequencyAction) $
    KMap.empty


-- ----------------------------------------------------------------------------
-- DemoJob
-- ----------------------------------------------------------------------------

  
demoJob :: JobInfo
demoJob = 
  createJobInfoFromConfiguration
    wordFrequencyAction -- action config
    ()                  -- options
    [("text1", "aaa bb c dd dd"),("text2", "aaa bb"),("text2", "aaa dd dd")] -- input (Tuples)
    []                  -- input (Files)
    1                   -- number of splitters
    2                   -- number of mappers
    1                   -- number of reducers
    1                   -- number of results
    TOTRawTuple         -- type of the result (file of raw)
{-  
  JobInfo 
  "demo-Word-Frequency-Job"
  (encode ())
  (Just $ JobAction "WORDFREQUENCY" TOTFile 5)
  (Just $ JobAction "WORDFREQUENCY" TOTFile 5)
  Nothing 
  (Just $ JobAction "WORDFREQUENCY" TOTRawTuple 1)
  (Just 1)
  ([TupleFunctionData (encodeTuple ("text1", "aaa bb c dd dd"))
   ,TupleFunctionData (encodeTuple ("text2", "aaa bb"))
   ,TupleFunctionData (encodeTuple ("text2", "aaa dd dd"))
   ,FileFunctionData "file1.txt"
   ])
-}


-- ----------------------------------------------------------------------------
-- DemoFiles
-- ----------------------------------------------------------------------------


createDemoFiles :: FS.FileSystem -> IO ()
createDemoFiles fs
  = do
    -- let c = S.BinaryFile (encode ("foo","a aa aaa b bb bbb"))
    -- let c = S.TextFile "harddisk file"
    let c = encode "harddisk file"
    FS.createFile "file1.txt" c fs