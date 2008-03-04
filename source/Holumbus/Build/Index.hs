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

import           Data.List
import           Data.Maybe

-- import           Holumbus.Index.Cache
import           Holumbus.Index.Common

import           System.Time

import           Text.XML.HXT.Arrow     -- import all stuff for parsing, validating, and transforming XML

-- -----------------------------------------------------------------------------

data IndexerConfig 
  = IndexerConfig
    { ic_startPages     :: [URI]
    , ic_tmpDump        :: Bool
    , ic_tmpPath        :: String
    , ic_idxPath        :: String
    , ic_contextConfigs :: [ContextConfig]
    , ic_fCrawlFilter   :: URI -> Bool
--    , ic_Attributes     :: Attributes 
    } 

data ContextConfig 
  = ContextConfig
    { cc_name           :: String
    , cc_preFilter      :: ArrowXml a => a XmlTree XmlTree
    , cc_XPath          :: String   -- multiple XPaths for one Context needed ???
    , cc_fTokenize      :: String -> [String]
    , cc_fIsStopWord    :: String -> Bool
    }
    
-- -----------------------------------------------------------------------------
    
indexMap :: [ContextConfig] -> Attributes -> String -> DocId -> String -> IO [(String, (String, String, DocId, Int))]
indexMap contextConfigs opts artificialKey docId theUri = do
    clt <- getClockTime
    cat <- toCalendarTime clt
    runX (  traceMsg 1 ((calendarTimeToString cat) ++ " - indexing document: " 
                                                   ++ show docId ++ " -> "
                                                   ++ show theUri)
        >>> processDocument opts contextConfigs (docId, theUri)
        >>> arr (\(c, w, d, p) -> (artificialKey, (c, w, d, p)))
      )
      
indexReduce :: HolIndex i => i -> String -> [(String, String, DocId, Int)] -> IO (Maybe i)
indexReduce idx _ l =
  return $! Just (foldl' theFunc idx l)
    where
    theFunc i (context, word, docId, pos) = insertPosition context word docId pos i
    
  
-- -----------------------------------------------------------------------------
    
processDocument :: 
     Attributes
  -> [ContextConfig]
  -> (DocId, String)
  -> IOSLA (XIOState s) b (Context, String, DocId, Int)
processDocument opts ccs (docId, url) =
        readDocument opts url
    >>> processContexts ccs docId
      
processContexts :: (ArrowXml a) =>
     [ContextConfig]
  -> DocId
  -> a XmlTree (Context, String, DocId, Int)
processContexts cc docId  = catA $ map (processContext docId) cc

    
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
      
      