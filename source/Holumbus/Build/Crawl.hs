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
  
  -- * Whatevering
  , tmpFile
  , initialCS
  )
where

import           Data.Binary
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Map    as M
import qualified Data.Set    as S
import qualified Data.IntMap as IM
import           Numeric

import           Holumbus.Build.Index
import           Holumbus.Index.Common
import           Holumbus.Index.Documents
import           Holumbus.Control.MapReduce.Parallel

import           Text.XML.HXT.Arrow     -- import all stuff for parsing, validating, and transforming XML
import           System.Time


type CustomFunc = (Arrow a) => a XmlTree b

-- | crawler state
data CrawlerState 
    = CrawlerState
      { cs_toBeProcessed    :: S.Set URI
      , cs_wereProcessed    :: S.Set URI
      , cs_unusedDocIds     :: [DocId]
      , cs_readAttributes   :: Attributes        -- passed to readDocument
      , cs_crawlFilter      :: (URI -> Bool)     -- decides if a link will be followed
      , cs_docs             :: Binary a => Documents a --IM.IntMap (Document String)
      , cs_tempPath         :: Maybe String     
      , cs_fGetCustom       :: CustomFunc
      }
      
-- | crawl a Web Page recursively
--   signature may change so that a doc table will be the computed result
crawl :: Int -> Int -> CrawlerState -> IO CrawlerState
crawl traceLevel maxWorkers cs = 
  if S.null ( cs_toBeProcessed cs )
    then return (cs)
    else do
                 -- get the Docs that have to be processed and add docIds
                 -- for the MapReduce Computation
         d    <- return $ cs_toBeProcessed cs
         d'   <- return $ zip (cs_unusedDocIds cs) (S.toList d)
         
         cs'  <- return $ cs { cs_unusedDocIds  = drop (S.size d) (cs_unusedDocIds cs)
                             , cs_wereProcessed = S.union d (cs_wereProcessed cs)
                             , cs_toBeProcessed = S.empty
                             }

         mr   <- mapReduce 
                    maxWorkers 
                    (crawlDoc traceLevel 
                              (cs_readAttributes cs) 
                              (cs_tempPath cs)
                              (cs_fGetCustom cs) ) 
                    (processCrawlResults cs)
                    d' 
                             
--         cs''<- return $! processCrawlResults (M.toList mr) cs'
         
         crawl traceLevel maxWorkers cs'
    
processCrawlResults :: Binary a =>
                       CrawlerState 
                    -> Int 
                    -> [(Maybe (Document a), [URI])] 
                    -> IO (Maybe CrawlerState)
processCrawlResults cs _ l =
    let
      newDocs        = S.unions (map S.fromList (map refs l))
      refs (_, uris) = filter (cs_crawlFilter cs) uris 
      processedDocs  = catMaybes $ filter isJust (map fst l)
    in      
      return $! Just 
        cs { cs_toBeProcessed = S.union
                                  (cs_toBeProcessed cs)
                                  (S.difference newDocs (cs_wereProcessed cs))
           , cs_docs          = theFold processedDocs
           } 
     where
     theFold :: Binary a => [(Document a)] -> Documents a
     theFold processedDocs
       = foldl' (\d r -> snd (insertDoc d r)) 
                             (cs_docs cs)
                              processedDocs

{-
processCrawlResults :: [((Int, Document String),  [URI])] -> CrawlerState -> CrawlerState
processCrawlResults l cs =
    let
      newDocs        = S.unions (map S.fromList (map refs l))
      refs (_, uris) = filter (cs_crawlFilter cs) uris 
    in      
     cs { cs_toBeProcessed = S.union
                                (cs_toBeProcessed cs)
                                (S.difference newDocs (cs_wereProcessed cs))
--        , cs_docMap      =  (IM.fromList processedDocs )
--                           `IM.union` 
--                            (cs_docMap cs)     
        } 
        where processedDocs = filter (\(_, doc) -> (title doc) /= errID && (uri doc) /= errID ) (map fst l)
        --where processedDocs = filter (\(_, doc) -> (title doc) /= errID && (uri doc) /= errID ) (map fst l)
-}

-- | Wrapper function for the "real" crawlDoc functions. The current time is
--   traced (to identify documents that take a lot of time to be processed while
--   testing) and the crawlDoc'-arrow is run
crawlDoc :: (Show t) => Int -> Attributes -> Maybe String -> CustomFunc -> t -> String -> IO [(Int, (Maybe (Document a), [String]))]
crawlDoc traceLevel attrs tmpPath getCustom docId theUri 
  = do
    clt <- getClockTime               -- get Clock Time for debugging
    cat <- toCalendarTime clt         -- and convert it to "real" date and time
    runX (     setTraceLevel traceLevel
           >>> traceMsg 1 (calendarTimeToString cat)  
           >>> (     constA 42        -- this is needed to fit the MapReduce abstraction
                 &&& crawlDoc' attrs tmpPath getCustom (docId, theUri)
               )   
          ) 
  
noReduce' :: k2 -> [v2] -> IO (Maybe v2)
noReduce' _ vs = do return $ Just (head vs)

-- | Download & read document and compute document title and included refs to
--   other documents 
crawlDoc' :: (Show t) =>
     Attributes    -- ^ options for readDocument
  -> Maybe String  -- ^ path for serialized tempfiles
  -> CustomFunc
  -> (t, String)   -- ^ DocId, URI
  -> IOSLA (XIOState s) b (Maybe (Document a), [String])
crawlDoc' attrs tmpPath getCustom (docId, theUri) =
        traceMsg 1 ("  crawling document: " ++ show docId ++ " -> " ++ show theUri )
    >>> readDocument attrs theUri
    >>> (
          documentStatusOk `guards`
          (
                ( 
                  (writeDocument [] ((fromMaybe "" tmpPath) ++ (tmpFile docId theUri)))
                  `whenP`
                   (const (isJust tmpPath )) 
                ) 
             >>> 
                (   
                  getDocument theUri getCustom 
                  &&&
                  (getLinks $< computeDocBase >>> strictA)
                ) 
          )
          `orElse` ( 
                clearErrStatus
            >>> traceMsg 0 (  "something went wrong with doc: " ++ theUri)    
            >>> constA (Nothing, [])
          )
        )

-- | extract the Title of a Document (for web pages the <title> tag) and combine
--   it with the document uri
getDocument :: (ArrowXml a) => 
--               Maybe String 
               URI 
            -> a XmlTree (Maybe b)
            -> a XmlTree (Maybe (Document b))
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
             
-- | Extract all Hyperlinks from a XmlTree
-- <br/>TODO: beautify & generalize protocol handling
-- <br/>TODO: beautify conversion from relative to absolute URLs
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
        proto `elem`  ["http", "https", "ftp", "file"]
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
-- <br/>Stolen from Uwe Schmidt
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
      

-- | create an initial CrawlerState from an IndexerConfig
initialCS :: IndexerConfig -> CustomFunc ->  CrawlerState
initialCS cic getCustom
  = CrawlerState
      (S.fromList (ic_startPages cic))
      S.empty
      [1..]
      (ic_readAttrs cic)
      (ic_fCrawlFilter cic)
      emptyDocuments
      (ic_tmpPath cic)
      getCustom           
          
        
-- | Computes a filename for a local temporary file.
--   Since filename computation might depend on the DocId it is also submitted
--   as a parameter
tmpFile :: Show t => t -> URI -> String
tmpFile _ []     = []
tmpFile _ (c:cs)
  = if isAlphaNum c || isSpace c 
      then c : tmpFile 42 cs
      else '%' : showHex (fromEnum c) "" ++ tmpFile 42 cs

{-
-- | crawl a Web Page recursively
--   signature may change so that a doc table will be the computed result
crawl :: Int -> Int -> CrawlerState -> IO CrawlerState
crawl traceLevel maxWorkers cs = 
  if S.null ( cs_toBeProcessed cs )
    then return (cs)
    else do
                 -- get the Docs that have to be processed and add docIds
                 -- for the MapReduce Computation
         d    <- return $ cs_toBeProcessed cs
         d'   <- return $ zip (cs_unusedDocIds cs) (S.toList d)
         
         cs'  <- return $ cs { cs_unusedDocIds  = drop (S.size d) (cs_unusedDocIds cs)
                             , cs_wereProcessed = S.union d (cs_wereProcessed cs)
                             , cs_toBeProcessed = S.empty
                             }

         mr   <- mapReduce 
                    maxWorkers 
                    (crawlDoc traceLevel (cs_readAttributes cs) (cs_tempPath cs) ) 
                    noReduce'
                    d' 
                             
         cs''<- return $! processCrawlResults (M.toList mr) cs'
         
         crawl traceLevel maxWorkers cs''

-- | Wrapper function for the "real" crawlDoc functions. The current time is
--   traced (to identify documents that take a lot of time to be processed while
--   testing) and the crawlDoc'-arrow is run
crawlDoc :: (Show t) => Int -> Attributes -> Maybe String -> t -> String -> IO [((t, Document String), [String])]
crawlDoc traceLevel attrs tmpPath docId theUri 
  = do
    clt <- getClockTime               -- get Clock Time for debugging
    cat <- toCalendarTime clt         -- and convert it to "real" date and time
    runX (     setTraceLevel traceLevel
           >>> traceMsg 1 (calendarTimeToString cat)  
           >>> crawlDoc' attrs tmpPath (docId, theUri) 
          )  
  
noReduce' :: k2 -> [v2] -> IO (Maybe v2)
noReduce' _ vs = do return $ Just (head vs)

-- | Download & read document and compute document title and included refs to
--   other documents 
crawlDoc' :: (Show t) =>
     Attributes   -- ^ options for readDocument
--  -> Bool         -- ^ write copies of data to hard disk ?
  -> Maybe String       -- ^ path for serialized tempfiles
  -> (t, String)  -- ^ DocId, URI
  -> IOSLA (XIOState s) b ((t, Document String), [String])
crawlDoc' attrs tmpPath (docId, theUri) =
        traceMsg 1 ("  crawling document: " ++ show docId ++ " -> " ++ show theUri )
    >>> readDocument attrs theUri
    >>> (
          documentStatusOk `guards`
          (
                ( 
                  (writeDocument [] ((fromMaybe "" tmpPath) ++ (tmpFile docId theUri)))
                  `whenP`
                   (const (isJust tmpPath )) 
                ) 
             >>> 
                (   
                  (constA docId
                  &&&
                  (getDocument Nothing theUri))
                  &&&
                  (getLinks $< computeDocBase >>> strictA)
                ) 
          )
          `orElse` ( 
                clearErrStatus
            >>> traceMsg 0 (  "something went wrong with doc: " ++ theUri)    
            >>> constA ((docId, (Document errID errID Nothing)), [])             -- TODO error handling
          )
        )
--    >>> strictA

errID :: String
errID = "__ERR__"   
    
processCrawlResults :: [((Int, Document String),  [URI])] -> CrawlerState -> CrawlerState
processCrawlResults l cs =
    let
      newDocs        = S.unions (map S.fromList (map refs l))
      refs (_, uris) = filter (cs_crawlFilter cs) uris 
    in      
     cs { cs_toBeProcessed = S.union
                                (cs_toBeProcessed cs)
                                (S.difference newDocs (cs_wereProcessed cs))
        , cs_docMap      =  (IM.fromList processedDocs )
                            `IM.union` 
                            (cs_docMap cs)     
        } 
        where processedDocs = filter (\(_, doc) -> (title doc) /= errID && (uri doc) /= errID ) (map fst l)
        --where processedDocs = filter (\(_, doc) -> (title doc) /= errID && (uri doc) /= errID ) (map fst l)
        
-- | extract the Title of a Document (for web pages the <title> tag) and combine
--   it with the document uri
getDocument :: (ArrowXml a) => Maybe String -> URI -> a XmlTree (Document String)
getDocument c theUri = getTitle >>> arr mkDoc
  where
  mkDoc :: String -> Document String
  mkDoc s = Document s theUri c
  getTitle :: (ArrowXml a) => a XmlTree String
  getTitle 
    =     getXPathTrees "/html/head/title/text()"
      >>> getText     
             
-- | Extract all Hyperlinks from a XmlTree
-- <br/>TODO: beautify & generalize protocol handling
-- <br/>TODO: beautify conversion from relative to absolute URLs
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
        proto `elem`  ["http", "https", "ftp", "file"]
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
-- <br/>Stolen from Uwe Schmidt
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
      

-- | create an initial CrawlerState from an IndexerConfig
initialCS :: IndexerConfig -> CustomFunc ->  CrawlerState
initialCS cic getCustom
  = CrawlerState
      (S.fromList (ic_startPages cic))
      S.empty
      [1..]
      (ic_readAttrs cic)
      (ic_fCrawlFilter cic)
      IM.empty
      (ic_tmpPath cic)
      getCustom           
          
        
-- | computes a filename for a local temporary file
tmpFile :: Show t => t -> URI -> String
tmpFile _ []     = []
tmpFile _ (c:cs)
  = if isAlphaNum c || isSpace c 
      then c : tmpFile 42 cs
      else '%' : showHex (fromEnum c) "" ++ tmpFile 42 cs

if x == '/'
                        then '_' : tmpFile docId xs
--                        then '%' : ('2' : ('F' : tmpFile docId xs))
                        else  
                          if (x == '#') || (x == ':') 
                            then '-' : ( '_' : ( '-' : tmpFile docId xs))
                            else x  : tmpFile docId xs
-}


         