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
{-# OPTIONS -fglasgow-exts #-}
-- -----------------------------------------------------------------------------

module Holumbus.Build.Crawl
  (
  -- * CrawlerState type
    CrawlerState (..)

  -- * Crawling
  , crawl
  
  -- * Initializing
  , initialCrawlerState
  
  )
where

import           Data.Binary
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Map    as M
import qualified Data.Set    as S

import           Holumbus.Build.Index
import           Holumbus.Control.MapReduce.Parallel
import           Holumbus.Index.Common
import           Holumbus.Index.Documents
import           Holumbus.Utility

import           Text.XML.HXT.Arrow
import           System.Time


type Custom b = IOSArrow XmlTree (Maybe b)

-- | crawler state
data CrawlerState b
    = CrawlerState
      { cs_toBeProcessed    :: S.Set URI
      , cs_wereProcessed    :: S.Set URI
      , cs_unusedDocIds     :: [DocId]        -- probably unneeded
      , cs_readAttributes   :: Attributes     -- passed to readDocument
      , cs_refXPaths        :: [String]       -- XPath expressions for references to other documents
      , cs_fCrawlFilter     :: (URI -> Bool)  -- decides if a link will be followed
      , cs_docs             :: Documents b       
      , cs_tempPath         :: Maybe String     
      , cs_fGetCustom       :: Custom b
      }
      
-- | The crawl function. MapReduce is used to scan the documents for references to other documents
--   that have to be added to the 'Documents', too.
crawl :: (Binary b) => Int -> Int -> CrawlerState b -> IO (Documents b)
crawl traceLevel maxWorkers cs = 
  if S.null ( cs_toBeProcessed cs ) -- if no more documents have to be processed, 
    then return (cs_docs cs)        -- the Documents are returned
    else do                         -- otherwise, the crawling is continued
                 -- get the Docs that have to be processed and add docIds
                 -- for the MapReduce Computation
         d    <- return $ cs_toBeProcessed cs
         d'   <- return $ zip (cs_unusedDocIds cs) (S.toList $ cs_toBeProcessed cs)
         
         cs'  <- return $ cs { cs_unusedDocIds  = drop (S.size d) (cs_unusedDocIds cs)
                             , cs_wereProcessed = S.union d (cs_wereProcessed cs)
                             , cs_toBeProcessed = S.empty
                             }

         mr   <- mapReduce maxWorkers (crawlDoc traceLevel cs) (processCrawlResults cs') d' 
         cs'' <-  return $! snd (M.elemAt 0 mr)                    
         
         crawl traceLevel maxWorkers cs''




-- | The REDUCE function for the crawling MapReduce computation. The 'Documents' and their contained
--   links are used to change the 'CrawlerState'. New documents are added to the list of 
--   unprocessed docs and the data of the already crawled documents are added to the 'Documents'. 
processCrawlResults :: (Binary b) => 
                       CrawlerState b                 -- ^ state before last MapReduce computation
                    -> Int                            -- ^ artificial key, only to fit MapReduce
                    -> [(Maybe (Document b), [URI])]  -- ^ data p in the crawl phase
                    -> IO (Maybe (CrawlerState b))
processCrawlResults cs _ l =
    let
        -- Concatenate reference lists of all documents and transform them into a set to eliminiate
        -- multiple occurences of one 'Document'
      newDocs        = S.unions (map S.fromList (map refs l))  
        -- filter new references with the crawl filter
      refs (_, uris) = filter (cs_fCrawlFilter cs) uris       
        -- take only documents that could be crawled without errors
      processedDocs  = catMaybes (map fst l)
    in      
      return $! Just 
        cs { cs_toBeProcessed = S.union                 -- add new documents to the todo list
                                  (cs_toBeProcessed cs)
                                  (S.difference newDocs (cs_wereProcessed cs))
           , cs_docs          = theFold processedDocs   -- insert crawled documents into 'Documents'
           } 
     where
     theFold processedDocs = foldl' (\d r -> snd (insertDoc d r)) (cs_docs cs) processedDocs





-- | Wrapper function for the "real" crawlDoc functions. The current time is
--   traced (to identify documents that take a lot of time to be processed while
--   testing) and the crawlDoc'-arrow is run
crawlDoc :: (Binary b) => 
            Int 
         -> CrawlerState b
         -> DocId 
         -> String 
         -> IO [(Int, (Maybe (Document b), [URI]))]
crawlDoc traceLevel cs docId theUri 
  = let attrs     = cs_readAttributes cs 
        tmpPath   = cs_tempPath cs
        refXPaths = cs_refXPaths cs
        getCustom = cs_fGetCustom cs 
    in
    do
    clt <- getClockTime               -- get Clock Time for debugging
    cat <- toCalendarTime clt         -- and convert it to "real" date and time
    runX (     setTraceLevel traceLevel
           >>> traceMsg 1 (calendarTimeToString cat)  
           >>> (     constA 42        -- this is needed to fit the MapReduce abstraction
                 &&& crawlDoc' attrs tmpPath refXPaths getCustom (docId, theUri)
               )   
          ) 

-- | Download & read document and compute document title and included refs to
--   other documents 
crawlDoc' :: (Binary b) =>
     Attributes    -- ^ options for readDocument
  -> Maybe String  -- ^ path for serialized tempfiles
  -> [String]
  -> Custom b
  -> (DocId, String)   -- ^ DocId, URI
  -> IOSArrow c (Maybe (Document b), [URI])
crawlDoc' attrs tmpPath refXPaths getCustom (docId, theUri) =
        traceMsg 1 ("  crawling document: " ++ show docId ++ " -> " ++ show theUri )
    >>> readDocument attrs theUri
    >>> (
          documentStatusOk `guards`   -- make sure that the document could be accessed
                -- if it is set in the crawler options, write a temporary copy of the document to
                -- the hard disk
          (     ( writeDocument [(a_indent, "1")] ((fromJust tmpPath) ++ (tmpFile docId theUri))
                  `whenP`
                  const (isJust tmpPath ) 
                ) 
                -- compute a pair of Document b (Holumbus datatype) and a list of contained links
            >>> (     getDocument theUri getCustom
                  &&& ( getRefs refXPaths >>> strictA )
                )
          )
          `orElse` (                  -- if an error occurs with the current document, the global 
                clearErrStatus        -- error status has to be reset, else the crawler would stop
            >>> traceMsg 0 (  "something went wrong with doc: " ++ theUri)    
            >>> constA (Nothing, [])  -- Nothing indicates the error, the empty list shows
          )                           -- that - caused by the error - no new links were found
        )

-- | extract the Title of a Document (for web pages the <title> tag) and combine
--   it with the document uri
getDocument :: (Binary b) => 
               URI        -- ^ URI of the 'Document' 
            -> Custom b   -- ^ Function to extract the custom Data from the 'Document'
            -> IOSArrow XmlTree (Maybe (Document b))
getDocument theUri getCustom 
  = mkDoc $<<< (     getTitle
                 &&& constA theUri
                 &&& getCustom
               )
    where
    mkDoc t u c = constA $ Just $ Document t u c
    getTitle 
      =     getXPathTrees "/html/head/title/text()"
        >>> getText     
             
-- | Extract References to other documents from a XmlTree based on configured XPath expressions
getRefs :: [String] -> IOSLA (XIOState s) XmlTree [URI]
getRefs xpaths
  = catA $ map getRefs' xpaths
    where getRefs' xpath = listA $ getXPathTrees xpath >>> getText >>> mkAbsURI


-- | create an initial CrawlerState from an IndexerConfig
initialCrawlerState :: (Binary b) => IndexerConfig -> Custom b -> CrawlerState b
initialCrawlerState cic getCustom
  = CrawlerState
    { cs_toBeProcessed  = S.fromList (ic_startPages cic)
    , cs_wereProcessed  = S.empty
    , cs_unusedDocIds   = [1..]
    , cs_readAttributes = ic_readAttrs cic
    , cs_refXPaths      = ["//a/@href/text()", "//frame/@src/text()", "//iframe/@src/text()"]
    , cs_fCrawlFilter   = ic_fCrawlFilter cic
    , cs_docs           = emptyDocuments
    , cs_tempPath       = ic_tmpPath cic
    , cs_fGetCustom     = getCustom
    }
    
          
          
        


         