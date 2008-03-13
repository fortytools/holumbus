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
    
-- -----------------------------------------------------------------------------

buildIndex :: HolIndex i => 
              Int -> Int -> [(DocId, String)] -> IndexerConfig -> i -> IO i
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
--   The first three
--   parameters have to be passed to the function to receive a function with a
--   valid MapReduce-map signature. <br/>
--   The function... TODO
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

    
-- | Process a Context. TODO ... 
processContext :: 
  (ArrowXml a) => 
     DocId
  -> ContextConfig
  -> a XmlTree (Context, String, DocId, Int)
processContext docId cc = 
        (cc_preFilter cc)
    >>> getXPathTreesInDoc (cc_XPath cc)
    >>> getText    

    -- TODO make this work - arrIO?
    -- >>> perform ( arr $ putDocText (createCache "/home/sms/cache/") (cc_name cc) docId ) 

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
                   [URI] 
                -> Maybe String 
                -> String 
                -> [ContextConfig] 
                -> Attributes 
--                -> a XmlTree b
                -> [String] 
                -> [String] 
                -> IndexerConfig
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
     