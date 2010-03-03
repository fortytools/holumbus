module Main
( main )
where

import Crawl2.Examples2.MRIndexer
import Crawl2.Examples2.Common
import Crawl2.Examples2.SimpleDMapReduceIO
import Holumbus.DCrawler.URIs
import Holumbus.DCrawler.Robots
import Holumbus.DCrawler.Core
import Holumbus.Common.Logging
import System.Log.Logger
import System.Environment
import System.Console.Readline

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
  putTimeStamp ("Client begin")
  ( baseUri : triplet : f : nf : [] ) <- getArgs
  initializeLogging [(localLogger, INFO),("measure",ERROR)]
  (index, _state) <- loop 0 (read triplet) 0 (emptyIndex, (firstState baseUri, M.empty)) (maybeStringlist f follow, maybeStringlist nf nofollow)
  writeToBinFile "index.bin" (ixs_index index)
  writeToXmlFile "index.xml" (ixs_index index)
  writeToBinFile "docs.bin"  (ixs_documents index)
  writeToXmlFile "docs.xml"  (ixs_documents index)  
  putTimeStamp ("Client end")
  
  where
  firstState baseUri = CrawlerState
    { cs_toBeProcessed    = singletonURIs baseUri
    , cs_newURIs = emptyURIs
    , cs_alreadyProcessed = emptyURIs    
    , cs_robots   = emptyRobots
    , cs_noOfDocs   = 0
    , cs_resultAccu = []
    }

loop :: Int -> (Int,Int,Int) -> DocId -> Result -> ([String],[String]) -> IO Result
loop count (splitters,mappers,reducers) nextId (index,state) followopts = do
  --_ <- readline "Press a key"
  infoM localLogger (show followopts)
  -- do another loop
  if (nullURIs . cs_toBeProcessed . fst) state
    then return (setLastId index (nextId-1),state)
    else iteration count (splitters,mappers,reducers) nextId (index,state) followopts 

iteration :: Int -> (Int,Int,Int) -> DocId -> Result -> ([String],[String]) -> IO Result
iteration count (splitters,mappers,reducers) nextId (index,(state,_)) followopts = do
--  infoM localLogger ("\n~~~~~~~~~\niteration: " ++ show count)  
  debugM localLogger ("\n\n++++\ntheToBeProcessed URIs: " ++ (show . cs_toBeProcessed $ state))  
  putTimeStamp ("iteration begin: " ++ show count)
  -- split toBeProcessed uris by num of workers tbps and give each uri an unique id :: [(URI, Int)]
  infoM localLogger "split the uris"
  let uris = toListURIs . cs_toBeProcessed $ state
      idoffset = length uris
      -- tbps = partition num (idoffset `div` num) (zip uris [nextId..])
      numbereduris = (zip uris [nextId..])
      tbps = partition' numbereduris [[] |_<- [1..splitters]] -- <<<<<<<<------------------------------- ??????????????
  debugM localLogger ("\n\n++++\npartitioned uris: " ++ show tbps)  
  infoM localLogger ("partitioned length: " ++ show (map length tbps))
  (appendFile "./uri.map" . unwords . map (\x -> show x ++ "\n")) numbereduris

  infoM localLogger "create states and assign map reduce keys to it"  
  let states = (map (:[]) . zip [0..splitters-1] . map (mkState (cs_alreadyProcessed state))) tbps -- [(Int,ResultState)] <<<<<<<<<<<<<<<<<<<
  debugM localLogger ("\n\n++++\nkeyd states: " ++ show states)    
  
  -- do the mr
  infoM localLogger ("do the mr: ")
  result <- client idxMap idxReduce followopts (splitters,mappers,reducers) states
  
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
  putTimeStamp ("iteration end: " ++ show count)
  loop (count+1) (splitters,mappers,reducers) (nextId+idoffset) (index',newState) followopts
  
{-
 leftfolds a list of result to one.
-}
mergeResults :: Result -> [Result] -> Result
mergeResults = L.foldl' (\(i', s') (i,s) -> (mergeIndices' i' i, mergeStates' s' s))

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
  debugM localLogger ("difference: " ++ show toBeProcessed''')    
  
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

maybeStringlist :: String -> [String] -> [String]
maybeStringlist s l
  | null s = l
  | otherwise = words s


nofollow :: [String]
nofollow =
  [
    ".*(pdf|PDF|jpg|gif|png|tar.gz|tgz|ppt|exe|txt|zip|doc|dot|ps|gz|nb|swf|JPG|tex|rss|mpg|mp3|m3u|java|svg|mdb|xls|docx|xslx|pptx|dta|lst|rar|avi|mp4)"
  , ".*/hdoc.*"
  , ".*/javadoc/.*"
  , ".*/java/.*/doc.*"
  , ".*/fileadmin/.*"
  , ".*/vorlesungen/c/beispiele/.*"
  , ".*/TclTk/program1.html"
  , ".*/~splan/.*"
  , ".*/~wk/.*"
  , ".*/~si/projekte/.*"
  , ".*/~si/vorlesungen/.*" -- makes strange base tags? 
  , ".*/archiv/.*"
  , ".*/src/.*"
  , ".*/news/.*"
  , ".*/\\?L=.*"
  ]
{-nofollow = ["tx_fhwunternehmensforum_pi3"                     -- deny
  , "http://asta.fh-wedel.de/.*"                -- slow
  , "http://biblserv.fh-wedel.de/.*"            -- slow
  , "http://darcs.fh-wedel.de/.*"               -- hackers only
  , "http://stud.fh-wedel.de/.*"                -- boring
  , "http://holumbus.fh-wedel.de/.*"
  , ".*/HXmlToolbox/hdoc.*", ".*/si/doc/javadoc/docs/.*"
  , ".*/java/jdk1.1.1/docs.*"
--                                       , "/~", "/%7E", "http://www.fh-wedel.de/mitarbeiter/"
  , ".*\\?L=0.*", ".*\\&L=0.*"
  , ".*.pdf$", ".*.jpg$", ".*.gif$", ".*.png$", ".*.tar.gz$"
  , ".*.ppt$", ".*.exe$", ".*.txt$", ".*.zip$", ".*.doc$"
  , ".*.dot$", ".*.png$", ".*.ps$", ".*.ps.gz$", ".*.nb$"
  , ".*.swf$", ".*.JPG$", ".*.tex$", ".*.rss$", ".*.mpg$"
  , ".*.mp3$", ".*.m3u$", ".*.java$", ".*.tgz$", ".*.svg", ".*.mdb$" 
  , ".*.PDF$", ".*.xls$", ".*.dta$", ".*.lst$", ".*.rar", ".*.avi$", ".*.mp4$" 
  , ".*%7Edi.*", ".*/~di.*"
  , ".*ws99/Ausarbeitung/mico/Beispiel.*"
  , ".*/rundgang/id=.*", ".*/vorlesungsplan/id=.*"
  , ".*/vorlesungsplan/sem=.*", ".*/tv-infosystem/.*", ".*/~splan/.*"
  , "http://www\\.fh-wedel\\.de/index\\.php\\?eID=tx_cms_showpic.*"
  , "http://www.fh-wedel.de/fileadmin/mitarbeiter/ne/CG/opengl_man.*"
  , "http://www.fh-wedel.de/%7Esi/vorlesungen/c/beispiele.*"
  , "http://www.fh-wedel.de/~si/vorlesungen/c/beispiele.*"
  , "http://www.fh-wedel.de/~wol/fhtml.*" -- very slow and boring pages
  , "http://www.fh-wedel.de/%7Esi/vorlesungen/internet/TclTk/program1.html" -- slow
  ]-}

follow :: [String]
follow = [ "http://www.fh-wedel.de/.*" ]
--follow = [ "http://www.fh-wedel.de/.*" ]
