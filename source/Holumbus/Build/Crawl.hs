-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Build.Crawl
  Copyright  : Copyright (C) 2008 Sebastian M. Schlatt
  License    : MIT
  
  Maintainer : Sebastian M. Schlatt (sms@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  This module provides some functions to crawl Web Pages recursively

-}

-- ----------------------------------------------------------------------------

module Holumbus.Build.Crawl where

import           Data.List
import           Data.Maybe
import qualified Data.Map    as M
import qualified Data.Set    as S

import           Holumbus.Index.Common
import           Holumbus.Control.MapReduce.Parallel

import           Text.XML.HXT.Arrow     -- import all stuff for parsing, validating, and transforming XML

data CrawlerState-- | internal crawler state 
    = CrawlerState
      { cs_toBeProcessed :: S.Set URI
      , cs_wereProcessed :: S.Set URI
      , cs_unusedDocIds  :: [DocId]
      , cs_readOptions   :: Attributes        -- passed to readDocument
      , cs_crawlFilter   :: (URI -> Bool)     -- decides if a link will be followed
      }

-- | Maximal parallel Threads. This will be replaced by configuration parameters
maxWorkers :: Int
maxWorkers = 5

-- | crawl a Web Page recursively
--   signature may change so that a doc table will be the computed result
crawl :: CrawlerState -> IO CrawlerState
crawl cs = 
  if S.null ( cs_toBeProcessed cs )
    then return (cs)
    else do
                 -- get the Docs that have to be processed and add docIds
                 -- for the MapReduce Computation
         d    <- return $ cs_toBeProcessed cs
         d'   <- return $ zip (cs_unusedDocIds cs) (S.toList d)
         
         cs' <- return $ cs { cs_unusedDocIds  = drop (S.size d) (cs_unusedDocIds cs)
                            , cs_wereProcessed = S.union d (cs_wereProcessed cs)
                            , cs_toBeProcessed = S.empty
                            }

         mr   <- mapReduce maxWorkers (crawlDoc (cs_readOptions cs)) noReduce d' 
         cs''<- return $ processCrawlResults (M.toList mr) cs'
         
         crawl cs''

-- | Download & read document and compute document title and included refs to
--   other documents 
crawlDoc :: (Show t) =>
     Attributes
  -> (t, String)
  -> IOSLA (XIOState s) b ((t, Document), [String])
crawlDoc opts (docId, uri) =
        traceMsg 1 ("  crawling document: " ++ show docId ++ " -> " ++ show uri )
    >>> readDocument opts uri
    >>> (
          ( documentStatusOk `guards`
--                writeTmpXmlFile stdOpts4Writing (tmpPath ++ "map/") uri
--            >>> 
                (   
                  (constA docId
                  &&&
                  (getDocument uri))
                  &&&
                  (getLinks $< computeDocBase >>> strictA)
                ) 
          ) `orElse` ( 
                clearErrStatus
            >>> constA ((docId, ("", "_ERR_")), [])             -- TODO error handling
          )
        )
--    >>> strictA
    
    
processCrawlResults :: [(t,  [URI])] -> CrawlerState -> CrawlerState
processCrawlResults l cs =
    cs {  
          cs_toBeProcessed = S.union
                                (cs_toBeProcessed cs)
                                (S.difference newDocs (cs_wereProcessed cs))
        } 
    where
      newDocs = S.unions (map S.fromList (map refs l))
      refs (_, uris) = filter (cs_crawlFilter cs) uris 
          
-- | extract the Title of a Document (for web pages the <title> tag) and combine
--   it with the document uri
getDocument :: (ArrowXml a) => URI -> a XmlTree Document
getDocument uri = getTitle &&& constA uri
  where
    getTitle :: (ArrowXml a) => a XmlTree String
    getTitle 
      =     getXPathTrees "/descendant::a/attribute::href/child::text()"
        >>> getText     
             
-- | Extract all Hyperlinks from a XmlTree
-- TODO: beautify & generalize protocol handling
-- TODO: beautify conversion from relative to absolute URLs
getLinks :: (ArrowXml a) => String -> a XmlTree [String]
getLinks base = listA $
--    ( 
           (getXPathTrees "/descendant::a/attribute::href/child::text()"    >>> getText  )
--       &&& (getXPathTrees "/descendant::frame/attribute::src/child::text()"  >>> getText )
--    )              --       ^-- frames
--    >>> arr (\(a,b) -> a++b)
    >>> isA supportedLink
    >>> arr toAbsRef
    where
      supportedLink ref
        = null rest
        ||
        proto `elem` ["http", "https", "ftp", "file"]
        where
            (proto, rest) = span (/= ':') ref

      toAbsRef ref
        = removeFragment $ fromMaybe ref $ expandURIString ref base
        where
            removeFragment r
              | "#" `isPrefixOf` path = reverse . tail $ path
              | otherwise = r
              where
                path = dropWhile (/='#') . reverse $ r       

-- | compute the base URL of a complete document with root node
-- The complete document URL is stored within the attributes of the
-- root node. The base for accessing referenced docs is computed
-- by takin into account the "base" tag "href" value
-- Stolen from Uwe Schmidt
computeDocBase  :: ArrowXml a => a XmlTree String
computeDocBase
    = ( ( ( this
      /> hasName "html"
      /> hasName "head"
      /> hasName "base"
      >>> getAttrValue "href"
    )
    &&&
    getAttrValue "transfer-URI"
  )
  >>> expandURI
      )
      `orElse`
      getAttrValue "transfer-URI"       
            
         