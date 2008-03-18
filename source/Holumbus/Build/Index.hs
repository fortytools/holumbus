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
import           Data.Maybe

-- import           Holumbus.Index.Cache
import           Holumbus.Control.MapReduce.Parallel
import           Holumbus.Index.Common

import           System.Time

import           Text.Regex
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
    , ic_readAttrs      :: Attributes
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
    }
    
-- | Merge Indexer Configs. Basically the first IndexerConfig is taken and
--   the startPages of all other Configs are added. The crawl filters are ORed
--   so that more pages might be indexed. So you better know what you are doing
--   when you are using this.
mergeIndexerConfigs :: IndexerConfig -> [IndexerConfig] -> IndexerConfig
mergeIndexerConfigs cfg1 [] = cfg1
mergeIndexerConfigs cfg1 (cfg2:cfgs) = mergeIndexerConfigs resCfg cfgs
  where 
  resCfg = IndexerConfig
      ((ic_startPages cfg1) ++ (ic_startPages cfg2))
      (ic_tmpPath cfg1)
      (ic_idxPath cfg1)
      (ic_contextConfigs cfg1)  -- cfg2, too?
      (\a -> (ic_fCrawlFilter cfg1) a || (ic_fCrawlFilter cfg2) a)
      (ic_readAttrs cfg1)
      
        
-- -----------------------------------------------------------------------------

-- | Build an Index over a list of Files.
buildIndex :: HolIndex i => 
              Int                -- ^ Number of parallel threads for MapReduce
           -> Int                -- ^ TraceLevel for Arrows
           -> [(DocId, String)]  -- ^ List of input Data
           -> IndexerConfig      -- ^ Configuration for the Indexing process
           -> i                  -- ^ An empty HolIndex. This is used to determine which kind of index to use.
           -> IO i               -- ^ returns a - hopefully informative - HolIndex
buildIndex workerThreads traceLevel docs idxConfig emptyIndex
  = do
    mr <- mapReduce workerThreads
                    (indexMap traceLevel
                              (ic_contextConfigs idxConfig) 
                              (ic_readAttrs      idxConfig)
                              "42"
                    )
                    (indexReduce emptyIndex) 
                    docs
    return $! snd (M.elemAt 0 mr)                       


-- | The MAP function a MapReduce computation for building indexes.
--   The first three parameters have to be passed to the function to receive
--   a function with a valid MapReduce-map signature. <br/>
--   The function optionally outputs some debug information and then starts
--   the processing of a file by passing it together with the configuration
--   for different contexts to the @processDocument@ function where the file
--   is read and then the interesting parts configured in the
--   context configurations are extracted.
indexMap :: Int -> [ContextConfig] -> Attributes -> String -> DocId -> String -> IO [(String, (String, String, DocId, Int))]
indexMap traceLevel contextConfigs opts artificialKey docId theUri = do
    clt <- getClockTime
    cat <- toCalendarTime clt
    runX (  
            setTraceLevel traceLevel
        >>> traceMsg 1 ((calendarTimeToString cat) ++ " - indexing document: " 
                                                   ++ show docId ++ " -> "
                                                   ++ show theUri)
        >>> processDocument opts contextConfigs (docId, theUri)
        >>> arr (\(c, w, d, p) -> (artificialKey, (c, w, d, p)))
      )
      
-- | The REDUCE function a MapReduce computation for building indexes.
--   Even though there might be faster ways to build an index, this function
--   works with completely on the HolIndex class functions. So it is possible
--   to use the Indexer with different Index implementations.
indexReduce :: HolIndex i => i -> String -> [(String, String, DocId, Position)] -> IO (Maybe i)
indexReduce idx _ l =
  return $! Just (foldl' theFunc idx l)
    where
    theFunc i (context, "",   docId, pos) = i -- insertPosition context "HIERISTDERFEHLER" docId pos i
    theFunc i (context, word, docId, pos) = insertPosition context word docId pos i
    
  
-- -----------------------------------------------------------------------------
    
-- | Downloads a document and calls the function to process the data for the
--   different contexts of the index
processDocument :: 
     Attributes
  -> [ContextConfig]
  -> (DocId, URI)
  -> IOSLA (XIOState s) b (Context, String, DocId, Int)
processDocument opts ccs (docId, theUri) =
        readDocument opts theUri
    >>> processContexts ccs docId
      
-- | Apply the processContext function to all configured contexts
processContexts :: (ArrowXml a) =>
     [ContextConfig]
  -> DocId
  -> a XmlTree (Context, String, DocId, Int)
processContexts cc docId  = catA $ map (processContext docId) cc

    
-- | Process a Context. Applies the given context to extract information from
--   the XmlTree that is passed in the arrow.
processContext :: 
  (ArrowXml a) => 
     DocId
  -> ContextConfig
  -> a XmlTree (Context, String, DocId, Int)
processContext docId cc = 
        (cc_preFilter cc)
    >>> getXPathTreesInDoc (cc_XPath cc)
    >>> getText    
    >>> arr (cc_fTokenize cc)
    >>> arr (filter (\s -> not ((cc_fIsStopWord cc) s)))
    >>> arr numberWords
    >>> arrL (tupelize (cc_name cc) docId )
    >>> strictA
    where
      tupelize context' docId' theWords    = map (mkTupel context' docId') theWords
      mkTupel  context' docId' (word, pos) = (context', word, docId', pos)
      numberWords :: [String] -> [(String, Int)]
      numberWords l = zip l [1..]    
      
-- -----------------------------------------------------------------------------
      
     
-- | Helper function for creating indexer configurations
mkIndexerConfig :: --(Arrow a, Binary b) =>
                   [URI]               -- ^ A list of URIs with which to start
                -> Maybe String        -- ^ Nothing = do not save tmp files locally. Just = Path where to save tmp files.         
                -> String              -- ^ Path where to save index, doctable & cache
                -> [ContextConfig]     -- ^ A list of Context Configurations 
                -> Attributes          -- ^ Attributes for readDocument
--                -> a XmlTree b
                -> [String]            -- ^ List of regular expressions for files that shall be indexed 
                -> [String]            -- ^ List of regular expressions for files that must not be indexed
                -> IndexerConfig       -- ^ Configuration for an Indexer
mkIndexerConfig startPages tmpPath idxPath contextConfigs attrs {-getCustom-} allow deny = 
  IndexerConfig
     startPages
     tmpPath
     idxPath
     contextConfigs
     (mkCrawlFilter allow deny) -- (const True)      
     attrs
--     getCustom
               
-- | Helper function to create Crawl filters based on regular expressions.
--   A excluding regular expression is always stronger than a including one.
mkCrawlFilter :: [String] -> [String] -> (URI -> Bool)
mkCrawlFilter as ds theUri = isAllowed && (not isForbidden ) 
         where
         isAllowed   = foldl (&&) True  (map (doesMatch theUri) as)
         isForbidden = foldl (||) False (map (doesMatch theUri) ds)
         doesMatch u a = isJust $ matchRegex (mkRegex $ a) u
     