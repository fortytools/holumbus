-- ----------------------------------------------------------------------------

{- |
  Module     : HayooIndexer
  Copyright  : Copyright (C) 2008 Sebastian M. Schlatt
  License    : MIT

  Maintainer : Sebastian M. Schlatt (sms@holumbus.org)
  Stability  : experimental
  Portability: untested
  Version    : 0.2.3

  Indexer for the Haskell API Documentation Search Engine Hayoo!

  Usage: Indexer [-c|-s|-i|-d]
    no options given: crawl, split and index
    -c: crawl
    -s: split
    -i: index
    -d: debug with limited document set (crawl, split and index)

  Current scope:
    - <http://hackage.haskell.org/>
    - <http://www.haskell.org/gtk2hs/docs/current/>
  
  TODO: 
  - Include class, data, ... descriptions.

-}

-- ----------------------------------------------------------------------------

{-# LANGUAGE Arrows #-}

module Main where

import           Hayoo.Common
import           Hayoo.Config
import           Hayoo.Split

-- import           Control.Monad hiding (join, when)
import qualified Control.Monad as CM (when)

import           Data.List
import           Data.Maybe


import           qualified Data.IntMap as IM
import           qualified Data.Map    as M

import           Holumbus.Build.Crawl
import           Holumbus.Build.Index

import           Holumbus.Control.MapReduce.Parallel
-- import           Holumbus.Index.Cache

import           Holumbus.Index.Common
import           Holumbus.Index.Inverted.Memory(emptyInverted)
import           Holumbus.Index.Cache
import           Holumbus.Index.Documents
import           Holumbus.Utility

import           System.Directory
import           System.Environment
import	         System.Exit

import           Text.XML.HXT.Arrow     
import           Text.Regex

main :: IO ()
main 
  = do

    argv <- getArgs
    if length argv > 1 then exitWith (ExitFailure (-1)) else return ()
    if (length argv == 1) && (not ((argv !! 0) `elem` ["-c", "-s", "-i", "-d"])) then exitWith (ExitFailure (-1)) else return ()
    -- ---------------------------------------------------------------------------------------------
    --   Configuration
    -- ---------------------------------------------------------------------------------------------
    dir <- getHomeDirectory

    let traceLevel    = 0
        workerThreads = 8 
        docsPerCrawl  = 256
        indexPath     = dir ++ "/indexes/hayoo"

        idxConfig     = 
          if (length argv) > 0 && (argv !! 0) == "-d"
            then ic_Holumbus  { ic_indexPath = dir ++ "/indexes/holumbus" }
            else ic_Hayoo { ic_indexPath = indexPath }
	
        additionalConfig = ic_Hayoo_additional { ic_indexPath    = indexPath } 
        
        crawlerState' = (initialCrawlerState idxConfig emptyDocuments customCrawlFunc)
        crawlerState  = crawlerState' {
          cs_fGetReferences = appendLinkUnifier (cs_fGetReferences crawlerState')
        }

    
      -- we don't want to shipwreck because of missing dirs
    createDirectoryIfMissing True  ((fromJust (ic_tempPath idxConfig)) ++ "split/")
    createDirectoryIfMissing True  (fromJust (ic_tempPath idxConfig))
    createDirectoryIfMissing False indexPath
    
    e <- doesFileExist (indexPath ++ "-cache.db")      -- remove cache if already existing
    CM.when e (removeFile (indexPath ++ "-cache.db"))
    
    
    -- ---------------------------------------------------------------------------------------------
    -- crawl or load documents from file 
    hayooDocs <- 
       -- find available documents and save local copies as configured 
       -- in the IndexerConfig
      if (length argv) == 0 || (argv !! 0) `elem` ["-c", "-d"]
      then 
        do
        runX (traceMsg 0 (" crawling  ----------------------------- " ))
        runX (traceMsg 0 ("           (1) Hackage " ))
        hackageCrawled <- crawl traceLevel workerThreads docsPerCrawl crawlerState 
        let hackageDocs = filterDocuments -- TODO clean this up
                            ( isPrefixOf "http://hackage.haskell.org/packages/archive/" . uri) $
                          filterDocuments 
                            (\d -> (not $ isSuffixOf "pkg-list.html" $ uri d))  $ 
                          filterDocuments 
                            (\d -> (not $ isSuffixOf "recent.html"  $ uri d )) 
                          hackageCrawled

            hackageLatest = filterDocuments (hackageFilter . uri) hackageDocs

        runX (traceMsg 0 ("           (2) Additional libraries " ))
        let additionalState = initialCrawlerState additionalConfig emptyDocuments customCrawlFunc
        additionalDocs <- 
          if (argv !! 0) == "-d"
            then return emptyDocuments
            else crawl traceLevel workerThreads docsPerCrawl additionalState
    
        let hayooDocs' = snd $ mergeDocs hackageLatest additionalDocs

        if (argv !! 0) == "-d" 
          then writeToXmlFile ( indexPath ++ "-predocs.xml") hayooDocs'
          else return ()
        writeToBinFile ( indexPath ++ "-predocs.bin") hayooDocs'
        return hayooDocs'
      else (loadFromBinFile ( indexPath ++ "-predocs.bin") :: IO (Documents FunctionInfo))

  
    hayooDocsSize <- return $! sizeDocs hayooDocs
    

    -- ---------------------------------------------------------------------------------------------
    -- split files or load documents from file
       -- split the locally saved documents into several small xml files
       -- where each file contains declaration & documentation of one function
    splitDocs <-
      if ((length argv) == 0 || (argv !! 0) `elem` ["-s", "-d"]) && hayooDocsSize > 0
      then
        do
        runX (traceMsg 0 (" splitting ----------------------------- " ))
        splitDocs'' <- mapReduce 
            workerThreads 
            (getVirtualDocs ((fromJust $ ic_tempPath idxConfig) ++ "split/"))
            mkVirtualDocList
            (IM.toList (IM.map uri (toMap hayooDocs)))
        splitDocs'  <-  return $! snd (M.elemAt 0 splitDocs'')
        if (argv !! 0) == "-d"
          then writeToXmlFile ( indexPath ++ "-docs.xml") (splitDocs')
          else return ()
        writeToBinFile ( indexPath ++ "-docs.bin") (splitDocs')
        return splitDocs'
      else
        if (argv !! 0) == "-i"
        then
          loadFromBinFile ( indexPath ++ "-docs.bin") :: IO (Documents FunctionInfo)
        else
          return emptyDocuments

    splitDocsSize <- return $! sizeDocs splitDocs

    -- ---------------------------------------------------------------------------------------------
    -- build an index over the split xml files if wanted
    
    if (length argv) == 0 || (argv !! 0) `elem` ["-i", "-d"]
      then
        do
        runX (traceMsg 0 (" indexing  ------------------------------ " ))
        localDocs <- return $ tmpDocs ((fromJust $ ic_tempPath idxConfig) ++ "split/") splitDocs
        cache     <- createCache ( indexPath ++ "-cache.db")
        idx       <- buildIndex workerThreads traceLevel localDocs idxConfig
                            emptyInverted (Just cache)
        if (argv !! 0) == "-d" then writeToXmlFile ( indexPath ++ "-index.xml") idx else return ()
        writeToBinFile ( indexPath ++ "-index.bin") idx
    
        -- ---------------------------------------------------------------------------------------------
        -- display some statistics
        putStrLn "---------------------------------------------------------------"
        putStrLn $ "Documents: " ++ show hayooDocsSize
        putStrLn $ "Split documents: " ++ show splitDocsSize
        putStrLn $ "Unique Words: " ++ show (sizeWords idx)    
        putStrLn "---------------------------------------------------------------"
        return ()
      else
        return ()


appendLinkUnifier :: ArrowXml a' => a' XmlTree [URI] -> a' XmlTree [URI]
appendLinkUnifier a = 
  a >>> arr (map (\s -> subRegex (mkRegex "http://hackage.haskell.org/packages/archive/([^/]*)/[^/]*/doc/html/(.*)") 
				 s
				 "http://hackage.haskell.org/packages/archive/\\1/latest/doc/html/\\2"))



-- -------------------------------------------------------------------------------------------------

-- | dummy function for the custom information in the Documents. This is used in the crawl phase
--   of the index generation. The real custom information is computed during the split phase where
--   every document is split up into pseude-documents which contain exactly one function
customCrawlFunc :: IOSArrow XmlTree (Maybe FunctionInfo)
customCrawlFunc = constA Nothing

    
