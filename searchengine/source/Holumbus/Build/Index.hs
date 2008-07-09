-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Build.Crawl
  Copyright  : Copyright (C) 2008 Sebastian M. Schlatt
  License    : MIT
  
  Maintainer : Sebastian M. Schlatt (sms@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  Indexer functions

-}

-- -----------------------------------------------------------------------------
{-# OPTIONS -fglasgow-exts #-}
-- -----------------------------------------------------------------------------

module Holumbus.Build.Index 
  (
  -- * Building indexes
    buildIndex
  , buildSplitIndex
  
  -- * Indexer Configuration
  , IndexerConfig (..)
  , ContextConfig (..)
  , mergeIndexerConfigs
  )

where

import           Data.List
import qualified Data.IntMap  as IM
import           Data.Maybe

import           Control.Exception
import           Control.Monad
import           Control.Parallel.Strategies

import           Holumbus.Build.Config
import           Holumbus.Control.MapReduce.ParallelWithClass
import           Holumbus.Index.Common
import           Holumbus.Index.Cache
import           Holumbus.Utility

import           System.Time

import           Text.XML.HXT.Arrow hiding (getXPathTrees)     -- import all stuff for parsing, validating, and transforming XML
import           Text.XML.HXT.Arrow.XPathSimple --(getXPathTrees)
-- -----------------------------------------------------------------------------

buildSplitIndex :: ( NFData i, HolDocuments d a, HolIndex i, XmlPickler i )=>
     Int
  -> Int
  -> d a
  -> IndexerConfig
  -> i
  -> Bool
  -> Int
  -> IO [String]
buildSplitIndex workerThreads traceLevel docs idxConfig emptyIndex buildCaches maxDocs
  = let docs' =  (map (\(i,d) -> (i, uri d)) (IM.toList $ toMap docs))
    in  buildSplitIndex' workerThreads traceLevel docs' idxConfig emptyIndex buildCaches maxDocs
     
    
buildSplitIndex' :: (NFData i, HolIndex i, XmlPickler i) =>
     Int
  -> Int
  -> [(DocId, URI)]
  -> IndexerConfig
  -> i
  -> Bool
  -> Int
  -> IO [String]
buildSplitIndex' workerThreads traceLevel docs idxConfig emptyIndex buildCaches maxDocs
  = do
    return $ assert ((sizeWords emptyIndex) == 0) Nothing  
    indexCount <- return $ ((length docs) `div` maxDocs) + 1 -- wrong
    docLists   <- return $ partitionList maxDocs docs
    configs    <- return $ map (\i -> idxConfig {ic_idxPath = (fromMaybe "/tmp/" (ic_tmpPath idxConfig)) ++ (show i) }) [1..indexCount]
    pathes     <- mapM build (zip configs docLists) -- caches)
    mergeCaches' ((ic_idxPath idxConfig) ++ "-cache.db") (map (\(i,cfg) -> (fromMaybe "/tmp/" (ic_tmpPath cfg)) ++ show i ++"-cache.db") (zip ([1..indexCount]) configs)) 
    return $ pathes
    where
      mergeCaches' :: String -> [String] -> IO ()
      mergeCaches' newCache oldCaches 
        = do
          new <- createCache newCache
          foldM mergeCaches'' new oldCaches 
          return()                           
      mergeCaches'' :: Cache -> String -> IO Cache
      mergeCaches'' c1 s 
        = do
          c2 <- createCache s
          mergeCaches c1 c2
      build :: (IndexerConfig, [(DocId, URI)]) -> IO (String)
      build (idxConfig', docs') 
        = do
          cache <- if buildCaches then mkCache (ic_idxPath idxConfig') else return $ Nothing
          idx   <- buildIndex' workerThreads traceLevel docs' idxConfig' emptyIndex cache
--          writeToXmlFile ( (ic_idxPath idxConfig') ++ "-index.xml") idx
          writeToBinFile ( (ic_idxPath idxConfig') ++ "-index.bin") idx
          return (ic_idxPath idxConfig')
      mkCache path
        = do 
          c <- createCache (path ++ "-cache.db")
          return $ Just c   
    
-- -----------------------------------------------------------------------------


buildIndex :: (HolDocuments d a, HolIndex i, HolCache c) => 
              Int                -- ^ Number of parallel threads for MapReduce
           -> Int                -- ^ TraceLevel for Arrows
           -> d a                -- ^ List of input Data
           -> IndexerConfig      -- ^ Configuration for the Indexing process
           -> i                  -- ^ An empty HolIndex. This is used to determine which kind of index to use.
           -> Maybe c
           -> IO i               -- ^ returns a HolIndex
buildIndex workerThreads traceLevel docs idxConfig emptyIndex cache
  = let docs' =  (map (\(i,d) -> (i, uri d)) (IM.toList $ toMap docs))
    in  buildIndex' workerThreads traceLevel docs' idxConfig emptyIndex cache

-- | Build an Index over a list of Files.
buildIndex' :: (HolIndex i, HolCache c) => 
              Int                -- ^ Number of parallel threads for MapReduce
           -> Int                -- ^ TraceLevel for Arrows
           -> [(DocId, String)]  -- ^ List of input Data
           -> IndexerConfig      -- ^ Configuration for the Indexing process
           -> i                  -- ^ An empty HolIndex. This is used to determine which kind of index to use.
           -> Maybe c
           -> IO i               -- ^ returns a HolIndex
buildIndex' workerThreads traceLevel docs idxConfig emptyIndex cache
  = do
    mr <- -- assert ((sizeWords emptyIndex) == 0) 
                 (mapReduce 
                    workerThreads
                    emptyIndex
                    (computePositions traceLevel
                              (isJust $ ic_tmpPath idxConfig)
                              (ic_contextConfigs idxConfig) 
                              (ic_readAttributes idxConfig)
                              cache
                    )
                    docs
                 )
    return mr
    -- return $! snd (M.elemAt 0 mr)                       


-- | The MAP function in a MapReduce computation for building indexes.
--   The first three parameters have to be passed to the function to receive
--   a function with a valid MapReduce-map signature.
--
--   The function optionally outputs some debug information and then starts
--   the processing of a file by passing it together with the configuration
--   for different contexts to the @processDocument@ function where the file
--   is read and then the interesting parts configured in the
--   context configurations are extracted.

computePositions :: HolCache c =>
               Int -> Bool -> [ContextConfig] -> Attributes -> Maybe c  
            -> DocId -> String -> IO [(String, (String, DocId, Int))]
computePositions traceLevel fromTmp contextConfigs attrs cache docId theUri
    = do
      clt <- getClockTime
      cat <- toCalendarTime clt
      runX (     setTraceLevel traceLevel
             >>> traceMsg 1 ((calendarTimeToString cat) ++ " - indexing document: " 
                                                        ++ show docId ++ " -> "
                                                        ++ show theUri)
             >>> processDocument traceLevel attrs' contextConfigs cache docId theUri
             >>> arr (\ (c, w, d, p) -> (c, (w, d, p)))
             >>> strictA
	   )
    where
    attrs' = if fromTmp
               then addEntries standardReadTmpDocumentAttributes attrs
               else attrs



{-      
-- | The REDUCE function in a MapReduce computation for building indexes.
--   Even though there might be faster ways to build an index, this function
--   works with completely on the HolIndex class functions. So it is possible
--   to use the Indexer with different Index implementations.
insertPositions :: (HolIndex i) => i -> String -> [(String, String, DocId, Position)] -> IO (Maybe i)
insertPositions idx _ l =
  return $! Just (foldl' theFunc idx l)
    where
    theFunc i (_, "", _ , _) = i -- TODO Filter but make sure that phrase searching is still possible 
    theFunc i (context, word, docId, pos) = insertPosition context word docId pos i
-}    
  
-- -----------------------------------------------------------------------------
    
-- | Downloads a document and calls the function to process the data for the
--   different contexts of the index
processDocument :: HolCache c =>  
     Int
  -> Attributes
  -> [ContextConfig]
  -> Maybe c
  -> DocId 
  -> URI
  -> IOSLA (XIOState s) b (Context, Word, DocId, Position)
processDocument traceLevel attrs ccs cache docId theUri =
        withTraceLevel (traceLevel - traceOffset) (readDocument attrs theUri)
    >>> (catA $ map (processContext cache docId) ccs )   -- process all context configurations  
    
-- | Process a Context. Applies the given context to extract information from
--   the XmlTree that is passed in the arrow.
processContext :: 
  ( HolCache c) => 
     Maybe c
  -> DocId
  -> ContextConfig
  -> IOSLA (XIOState s) XmlTree (Context, Word, DocId, Position)
{-
processContext cache docId cc  = 
        (cc_preFilter cc)                                     -- convert XmlTree
    >>> listA (
                    getXPathTrees (cc_XPath cc)               -- extract interesting parts
                >>> deep isText                               -- Search deep for text nodes
                >>> getText 
              )
    >>> arr concat                                            -- convert text nodes into strings
    >>> arr (cc_fTokenize cc)                                 -- apply tokenizer function
    >>> arr (filter (\w -> w /= ""))                          -- filter empty words
    >>> perform ( (const $ (isJust cache) && (cc_addToCache cc)) -- write cache data if configured
                  `guardsP` 
                   ( arr unwords >>> arrIO ( putDocText (fromJust cache) (cc_name cc) docId)) 
                )    
    >>> arr (zip [1..])                                       -- number words
    >>> arr (filter (\(_,s) -> not ((cc_fIsStopWord cc) s)))  -- remove stop words
    >>> arrL (map (\(p, w) -> (cc_name cc, w, docId, p) ))    -- make a list of result tupels
    >>> strictA                                               -- force strict evaluation
-}

processContext cache docId cc
    = cc_preFilter cc                                         -- convert XmlTree
      >>>
      fromLA extractWords
      >>>
      ( if ( isJust cache
	     &&
	     cc_addToCache cc
	   )
	then perform (arrIO storeInCache)
	else this
      )
      >>>
      arrL genWordList
      >>>
      strictA
    where
    extractWords	:: LA XmlTree [String]
    extractWords
	= listA
	  ( xshow ( getXPathTrees (cc_XPath cc)               -- extract interesting parts
		    >>>
		    getTexts
		  )
	    >>>
	    arrL ( filter (not . null) . cc_fTokenize cc )
	  )

    genWordList		:: [String] -> [(Context, Word, DocId, Position)]
    genWordList
	= zip [1..]                                           -- number words
	  >>>                                                 -- arrow for pure functions
	  filter (not . (cc_fIsStopWord cc) . snd)            -- delete boring words
	  >>>
	  map ( \ (p, w) -> (cc_name cc, w, docId, p) )       -- attach context and docId

    storeInCache s
	= let t = unwords s in 
        if t /= "" then putDocText (fromJust cache) (cc_name cc) docId t
                   else return()

getTexts	:: LA XmlTree XmlTree
getTexts                                                      -- select all text nodes
    =  choiceA
       [ isElem :-> ( space                                   -- substitute tags by a single space
		      <+>                                     -- so tags act as word delimiter
		      (getChildren >>> getTexts)
		      <+>
		      space
		    )	                                      -- tags are interpreted as word delimiter
       , isText :-> this				      -- take the text nodes
       , this   :-> none				      -- ignore anything else
       ]
    where
    space = txt " "
               
     