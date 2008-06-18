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

module Holumbus.Build.Index where

-- import           Data.Binary
import           Data.List
import qualified Data.Map     as M
import qualified Data.IntMap  as IM
import           Data.Maybe
import           Control.Exception
import           Control.Monad
-- import           Holumbus.Index.Cache
import           Holumbus.Control.MapReduce.ParallelOld
import           Holumbus.Index.Common
import           Holumbus.Index.Cache
-- import qualified Holumbus.Index.Documents as DOC
import Control.Parallel.Strategies
import           Holumbus.Utility

import           System.Time

import           Text.XML.HXT.Arrow     -- import all stuff for parsing, validating, and transforming XML

-- -----------------------------------------------------------------------------

-- | Configuration for the indexer. 
data IndexerConfig 
  = IndexerConfig
    { ic_startPages     :: [URI]
    , ic_tmpPath        :: Maybe String   
    , ic_idxPath        :: String
    , ic_contextConfigs :: [ContextConfig]
    , ic_fCrawlFilter   :: URI -> Bool     -- will be passed to crawler, not needed for indexing
    , ic_readAttributes :: Attributes
--    , ic_fGetCustom     :: (Arrow a, Binary b) => a XmlTree b
    } 

-- | Configuration for a Context. It has a name with which it will be identified
--   as a index part. The preFilter is applied to the XmlTree that is generated
--   by the parser and before the "interesting" document parts are selected by
--   the XPath Expression. The Tokenize functions defines how a string found by
--   the XPath Expression will be split into a list of Words. Since stopwords
--   are identified by a function it is possible to define a list of words that
--   shall not be indexed or a function that excludes words because of the count
--   of characters or a combination of both.
data ContextConfig 
  = ContextConfig
    { cc_name           :: String
    , cc_preFilter      :: ArrowXml a => a XmlTree XmlTree
    , cc_XPath          :: String             -- multiple XPaths for one Context needed ???
    , cc_fTokenize      :: String -> [String]
    , cc_fIsStopWord    :: String -> Bool
    , cc_addToCache     :: Bool
    }
    
    
buildSplitIndex :: (NFData i, HolDocuments d a, HolIndex i, XmlPickler i) =>
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
    configs    <- return $ map (\i -> idxConfig {ic_idxPath = (ic_idxPath idxConfig) ++ (show i) }) [1..indexCount]
    pathes     <- mapM build (zip configs docLists) -- caches)
    mergeCaches' ((ic_idxPath idxConfig) ++ "-cache.db") (map (\cfg -> (ic_idxPath cfg) ++ "-cache.db") configs) 
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
          return $! rnf idx
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
    mr <- assert ((sizeWords emptyIndex) == 0) 
                 (mapReduce workerThreads
                    (indexMap traceLevel
                              (ic_contextConfigs idxConfig) 
                              (ic_readAttributes idxConfig)
                              cache
                              "42" -- this is an artificial key to fit the MapReduce abstraction
                    )
                    (indexReduce emptyIndex) 
                    docs
                 )
    return $! snd (M.elemAt 0 mr)                       


-- | The MAP function in a MapReduce computation for building indexes.
--   The first three parameters have to be passed to the function to receive
--   a function with a valid MapReduce-map signature. <br/>
--   The function optionally outputs some debug information and then starts
--   the processing of a file by passing it together with the configuration
--   for different contexts to the @processDocument@ function where the file
--   is read and then the interesting parts configured in the
--   context configurations are extracted.
indexMap :: HolCache c =>
               Int -> [ContextConfig] -> Attributes -> Maybe c -> String 
            -> DocId -> String -> IO [(String, (String, String, DocId, Int))]
indexMap traceLevel contextConfigs attrs cache artificialKey docId theUri = do
    clt <- getClockTime
    cat <- toCalendarTime clt
    runX (  
            setTraceLevel traceLevel
        >>> traceMsg 1 ((calendarTimeToString cat) ++ " - indexing document: " 
                                                   ++ show docId ++ " -> "
                                                   ++ show theUri)
        >>> processDocument traceLevel attrs contextConfigs cache docId theUri
        >>> arr (\(c, w, d, p) -> (artificialKey, (c, w, d, p)))
        >>> strictA
      )
      
-- | The REDUCE function in a MapReduce computation for building indexes.
--   Even though there might be faster ways to build an index, this function
--   works with completely on the HolIndex class functions. So it is possible
--   to use the Indexer with different Index implementations.
indexReduce :: (HolIndex i) => i -> String -> [(String, String, DocId, Position)] -> IO (Maybe i)
indexReduce idx _ l =
  return $! Just (foldl' theFunc idx l)
    where
    theFunc i (_, "", _ , _) = i -- TODO Filter but make sure that phrase searching is still possible 
    theFunc i (context, word, docId, pos) = insertPosition context word docId pos i
    
  
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
processContext cache docId cc  = 
        (cc_preFilter cc)                                     -- convert XmlTree
    >>> listA (
                    getXPathTrees (cc_XPath cc)               -- extract interesting parts
                >>> deep isText                               -- Search deep for text nodes
                >>> getText 
              )
    >>> arr concat                                            -- convert text nodes into strings
    >>> arr (cc_fTokenize cc)                                 -- apply tokenizer function
    >>> perform ( (const $ (isJust cache) && (cc_addToCache cc)) -- write cache data if configured
                  `guardsP` 
                   ( arr unwords >>> arrIO ( putDocText (fromJust cache) (cc_name cc) docId)) 
                )    
    >>> arr (zip [1..])                                       -- number words
    >>> arr (filter (\(_,s) -> not ((cc_fIsStopWord cc) s)))  -- remove stop words
    >>> arrL (map (\(p, w) -> (cc_name cc, w, docId, p) ))    -- make a list of result tupels
    >>> strictA                                               -- force strict evaluation
    where
      theTrace s = traceMsg 0 s -- >>> constA s
-- -----------------------------------------------------------------------------
-- | Merge Indexer Configs. Basically the first IndexerConfig is taken and
--   the startPages of all other Configs are added. The crawl filters are OR-ed
--   so that more pages might be indexed. So you better know what you are doing
--   when you are using this.

mergeIndexerConfigs :: IndexerConfig -> IndexerConfig -> IndexerConfig
mergeIndexerConfigs cfg1 cfg2 = mergeIndexerConfigs' cfg1 [cfg2]

mergeIndexerConfigs' :: IndexerConfig -> [IndexerConfig] -> IndexerConfig
mergeIndexerConfigs' cfg1 [] = cfg1
mergeIndexerConfigs' cfg1 (cfg2:cfgs) = mergeIndexerConfigs' resCfg cfgs
  where 
  resCfg = IndexerConfig
      ((ic_startPages cfg1) ++ (ic_startPages cfg2))
      (ic_tmpPath cfg1)
      (ic_idxPath cfg1)
      (ic_contextConfigs cfg1)  -- cfg2, too?
      (\a -> (ic_fCrawlFilter cfg1) a || (ic_fCrawlFilter cfg2) a)
      (ic_readAttributes cfg1)
   
                   
               
     