-- ----------------------------------------------------------------------------
{- |
  Module     : Examples.MapReduce.WordFrequency.WordFrequency
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Examples.MapReduce.WordFrequency.WordFrequency
(
  wordFrequencyAction
, wordFrequencyActionMap
, wordFrequencyDemoJob
, createWordFrequencyDemoFiles
, textList
)
where

import           Data.Binary

import           System.Log.Logger

import qualified Holumbus.FileSystem.FileSystem as FS

import qualified Holumbus.Data.AccuMap as AMap
import qualified Holumbus.Data.KeyMap as KMap
import           Holumbus.MapReduce.Types


localLogger :: String
localLogger = "Examples.MapReduce.WordFrequency.WordFrequency"


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

wordFrequencyActionMap :: ActionMap
wordFrequencyActionMap
  = KMap.insert (readActionConfiguration wordFrequencyAction) $
    KMap.empty


-- ----------------------------------------------------------------------------
-- DemoJob
-- ----------------------------------------------------------------------------

  
wordFrequencyDemoJob :: JobInfo
wordFrequencyDemoJob = 
  createJobInfoFromConfiguration
    wordFrequencyAction -- action config
    ()                  -- options
    textList            -- input (Tuples)
    []                  -- input (Files)
    1                   -- number of splitters
    2                   -- number of mappers
    1                   -- number of reducers
    1                   -- number of results
    TOTRawTuple         -- type of the result (file of raw)
    
    
textList :: [(String,String)]
textList
  = [("text1", "aaa bb c dd dd"),("text2", "aaa bb"),("text2", "aaa dd dd")]


-- ----------------------------------------------------------------------------
-- DemoFiles
-- ----------------------------------------------------------------------------


createWordFrequencyDemoFiles :: FS.FileSystem -> IO ()
createWordFrequencyDemoFiles fs
  = do
    let c = encode "harddisk file"
    FS.createFile "file1.txt" c fs