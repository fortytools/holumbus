module Main
( main )
where

import Examples2.Re.MRIndexer
import Examples2.Re.Common
import Holumbus.Distribution.SimpleDMapReduceIO
import Holumbus.DCrawler.URIs
import Holumbus.DCrawler.Robots
import Holumbus.DCrawler.Core
import Holumbus.Common.Logging
import System.Log.Logger
import System.Environment

import Holumbus.Index.Common hiding (URI)
import Holumbus.Index.Documents (Documents(..))
import Holumbus.DCrawler.IndexerCore

import qualified Data.List as L
import qualified Data.Map as M

localLogger :: String
localLogger = "Main"

{-
First run:
toBeProcessed URIS: start
alreadyProcessed URIs: none

Second run:
toBeProcessed URIS: all new uri
alreadyProcessed URIs: start

split all new uris up. use num of mappers for that, so that each mapper gets various uris.
set num of documents to sizeof uris

-}

main :: IO ()
main = do 
  ( baseUri : follow : num : [] ) <- getArgs
  initializeLogging [(localLogger, INFO)]
  (index, _state) <- loop 0 (read num) 0 (emptyIndex, (firstState baseUri, M.empty)) follow
  writeToBinFile "index.bin" (ixs_index index)
  writeToXmlFile "index.xml" (ixs_index index)
  writeToBinFile "docs.bin"  (ixs_documents index)
  writeToXmlFile "docs.xml"  (ixs_documents index)  
  
  where
  firstState baseUri = CrawlerState
    { cs_toBeProcessed    = singletonURIs baseUri
    , cs_newURIs = emptyURIs
    , cs_alreadyProcessed = emptyURIs    
    , cs_robots   = emptyRobots
    , cs_noOfDocs   = 0
    , cs_resultAccu = []
    }

loop :: Int -> Int -> DocId -> Result -> String -> IO Result
loop count num nextId (index,state) follow = do
  -- do another loop
  if (nullURIs . cs_toBeProcessed . fst) state
    then return (setLastId index (nextId-1),state)
    else iteration count num nextId (index,state) follow 

iteration :: Int -> Int -> DocId -> Result -> String -> IO Result
iteration count num nextId (index,(state,_)) follow = do
  infoM localLogger ("\n~~~~~~~~~\niteration: " ++ show count)  
  debugM localLogger ("\n\n++++\ntheToBeProcessed URIs: " ++ (show . cs_toBeProcessed $ state))  
  -- split toBeProcessed uris by num of workers tbps and give each uri an unique id :: [(URI, Int)]
  infoM localLogger "split the uris"
  let uris = toListURIs . cs_toBeProcessed $ state
      idoffset = length uris
      tbps = partition num (idoffset `div` num) (zip uris [nextId..])
  debugM localLogger ("\n\n++++\npartitioned uris: " ++ show tbps)  

  infoM localLogger "create states and assign map reduce keys to it"  
  let states = (zip [0..num-1] . map (mkState (cs_alreadyProcessed state))) tbps -- [(Int,ResultState)]
  debugM localLogger ("\n\n++++\nkeyd states: " ++ show states)    
  
  -- do the mr
  infoM localLogger ("do the mr: ")
  result <- client idxMap idxReduce follow num states  
  
  -- merge results
  infoM localLogger "merge the results"
  let (index', state') = (mergeResults (index, emptyState) . map snd) result

  -- debugging stuff ------------------------------------------------------------------------------------
  debugM localLogger ("\n\n++++\nstate tbp is: " ++ (show . cs_toBeProcessed) state ++ "\n++\n")
  debugM localLogger ("\n\n++++\nstate new is: " ++ (show . cs_newURIs) state++ "\n++\n")  
  debugM localLogger ("\n\n++++\nstate ap is: " ++ (show . cs_alreadyProcessed) state++ "\n++\n")  
  debugM localLogger ("\n\n++++\nstate' tbp is: " ++ (show . cs_toBeProcessed . fst) state'++ "\n++\n")
  debugM localLogger ("\n\n++++\nstate' new is: " ++ (show . cs_newURIs . fst) state' ++ "\n++\n")  
  debugM localLogger ("\n\n++++\nstate' ap is: " ++ (show . cs_alreadyProcessed . fst) state'++ "\n++\n")    
  -- ----------------------------------------------------------------------------------------------------
  
  -- make a new state with merged uris
  newState <- returnState (state,snd state') state'
  
  -- debugging stuff ------------------------------------------------------------------------------------
  debugM localLogger ("\n\n++++\nnewstate tbp is: " ++ (show . cs_toBeProcessed . fst) newState++ "\n++\n")
  debugM localLogger ("\n\n++++\nnewstate new is: " ++ (show . cs_newURIs . fst) newState++ "\n++\n")     
  debugM localLogger ("\n\n++++\nnewstate ap is: " ++ (show . cs_alreadyProcessed . fst) newState++ "\n++\n")    
  -- ----------------------------------------------------------------------------------------------------
  
  -- iterate
  loop (count+1) num (nextId+idoffset) (index',newState) follow
  
{-
 leftfolds a list of result to one.
-}
mergeResults :: Result -> [Result] -> Result
mergeResults = L.foldl' (\(i', s') (i,s) -> (mergeIndices' i' i, mergeStates' s' s))

{-
  partition
  fst Int -> how many list do we want to have?
  snd Int -> how many elements should each list have?
-}
partition :: Int -> Int -> [a] -> [[a]]
partition 1     _ ls = [ls]
partition _     _ [] = []
partition count i ls = first : (partition (count-1) i rest)
  where
  (first,rest) = splitAt i ls

{-
  copystate and do a merging over all tbp, new and ap uris.
-}
returnState :: ResultState -> ResultState -> IO ResultState
returnState (state,map') (state',_map) = do   
  -- union already processed uris
  infoM localLogger "union already processed"    
  let alreadyProcessed' = unionURIs (cs_alreadyProcessed state') (cs_alreadyProcessed state)
  debugM localLogger ("union already processed: " ++ show alreadyProcessed')

  -- union to be processed uris
  infoM localLogger "union to be processed"    
  let toBeProcessed' = unionURIs (cs_toBeProcessed state') (cs_toBeProcessed state)
  debugM localLogger ("union to be processed: " ++ show toBeProcessed')  

  -- union to be processed uris and the new uris
  infoM localLogger "union to be processed and new uris"    
  let toBeProcessed'' = unionURIs toBeProcessed' (cs_newURIs state')
  debugM localLogger ("tbp and new: " ++ show toBeProcessed'')  
  
  -- substract already processed uris from to be processed uris
  infoM localLogger "substract already processed uris from to be processed uris"  
  let toBeProcessed''' = diffURIs toBeProcessed'' alreadyProcessed'
  debugM localLogger ("difference: " ++ show toBeProcessed'')    
  
  -- return the new state
  return (CrawlerState {
      cs_toBeProcessed    = toBeProcessed'''
    , cs_newURIs = emptyURIs
    , cs_alreadyProcessed = alreadyProcessed'
    , cs_robots   = M.union c c'
    , cs_noOfDocs   = d + d'
    , cs_resultAccu = e ++ e'
    }, map')
  where
    -- get fields from state
    c  = cs_robots state
    d  = cs_noOfDocs state
    e  = cs_resultAccu state
    -- get fields from state'
    c' = cs_robots state'
    d' = cs_noOfDocs state'
    e' = cs_resultAccu state'    


{-
  creates a resultstate with given uris and uris2idmap
-}    
mkState :: URIs -> [(URI,Int)] -> ResultState
mkState uriset urilist = ((initCrawlerState []) { cs_alreadyProcessed = uriset}, M.fromList urilist)

{-
  set the last id used in the documents
-}
setLastId :: ResultIndex -> DocId -> ResultIndex
setLastId IndexerState { ixs_index=idx, ixs_documents=docs} id' = IndexerState { ixs_index = idx, ixs_documents = setLastId' docs }
  where
  setLastId' :: Documents a -> Documents a
  setLastId' Documents {idToDoc = a,docToId=b,lastDocId=_} = Documents {idToDoc = a,docToId=b,lastDocId=id'}
