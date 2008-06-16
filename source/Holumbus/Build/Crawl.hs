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
{-# OPTIONS -fglasgow-exts -fno-warn-orphans #-}
-- -----------------------------------------------------------------------------

module Holumbus.Build.Crawl
  (
  -- * CrawlerState type
    CrawlerState (..)

  -- * Crawling
  , crawl
  
  -- * Initializing
  , initialCrawlerState
  , getRefs
  
  -- * Persistence
  , loadCrawlerState
  , saveCrawlerState
  )
where

import           Control.Monad

import           Data.Binary
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Map    as M
import qualified Data.Set    as S
import qualified Data.IntMap as IM
import           Data.Digest.MD5
import           Data.ByteString.Lazy.Char8(pack)

import           Holumbus.Build.Index
import           Holumbus.Control.MapReduce.ParallelOld
import           Holumbus.Index.Common
import           Holumbus.Index.Documents
import           Holumbus.Utility

import           Text.XML.HXT.Arrow
import           System.Time

-- import Control.Parallel.Strategies


type Custom a = IOSArrow XmlTree (Maybe a)

-- | crawler state
data CrawlerState a
    = CrawlerState
      { cs_toBeProcessed    :: S.Set URI
      , cs_wereProcessed    :: S.Set URI
      , cs_unusedDocIds     :: [DocId]        -- probably unneeded
      , cs_readAttributes   :: Attributes     -- passed to readDocument
      , cs_fPreFilter       :: ArrowXml a' => a' XmlTree XmlTree  -- filter that is applied before
      , cs_refXPaths        :: [String]       -- XPath expressions for references to other documents
      , cs_fCrawlFilter     :: (URI -> Bool)  -- decides if a link will be followed
      , cs_docs             :: Documents a       
      , cs_tempPath         :: Maybe String     
      , cs_fGetCustom       :: Custom a
      , cs_docHashes        :: M.Map String URI
      }

instance Binary a => Binary (CrawlerState a) where
  put (CrawlerState tbp wp _ _ _ _ _ d _ _ dh) 
    = put tbp >> put wp >> put d >> put dh
      
  get = do
        tbp <- get
        wp  <- get
        d   <- get
        dh  <- get
        return $ CrawlerState tbp wp (ids d) [] this [] (const False) d Nothing (constA Nothing) dh
        where
          ids :: Documents a -> [Int]
          ids d =  [1..] \\ (IM.keys $ toMap d) 

-- | The crawl function. MapReduce is used to scan the documents for references to other documents
--   that have to be added to the 'Documents', too.
crawl :: (Binary b, XmlPickler b) => Int -> Int -> Int -> CrawlerState b -> IO (Documents b)
crawl traceLevel maxWorkers maxDocs cs = 
  if S.null ( cs_toBeProcessed cs ) -- if no more documents have to be processed, 
    then return (cs_docs cs)        -- the Documents are returned
    else do                         -- otherwise, the crawling is continued
                 -- get the Docs that have to be processed and add docIds
                 -- for the MapReduce Computation
         d    <- return $ (S.fromList . (take maxDocs) . S.toList) (cs_toBeProcessed cs)
         d'   <- return $ zip (cs_unusedDocIds cs) (S.toList d) -- (S.toList $ cs_toBeProcessed cs)
         
         saveCrawlerState "/tmp/CrawlerState.bin" cs
         writeToXmlFile   "/tmp/Docs.xml" (cs_docs cs)
                                      
         runX (traceMsg 0 ("          Status: already processed: " ++ show (S.size $ cs_wereProcessed cs) ++ 
                           ", to be processed: "   ++ show (S.size $ cs_toBeProcessed cs)))
         
         cs'  <- return $ cs { cs_unusedDocIds  = drop (S.size d) (cs_unusedDocIds cs)
                             , cs_wereProcessed = S.union d (cs_wereProcessed cs)
                             , cs_toBeProcessed = S.difference (cs_toBeProcessed cs) d
                             }

         mr   <- mapReduce maxWorkers (crawlDoc traceLevel cs) (processCrawlResults cs') d' 
                             
         crawl traceLevel maxWorkers maxDocs (snd $ M.elemAt 0 mr)




-- | The REDUCE function for the crawling MapReduce computation. The 'Documents' and their contained
--   links are used to modify the 'CrawlerState'. New documents are added to the list of 
--   unprocessed docs and the data of the already crawled documents are added to the 'Documents'. 
processCrawlResults :: (Binary b) => 
                       CrawlerState b                 -- ^ state before last MapReduce computation
                    -> Int                     -- ^
                    -> [(String, Maybe (Document b), S.Set URI)]  -- ^ data produced in the crawl phase
                    -> IO (Maybe (CrawlerState b))
processCrawlResults oldCs _ l = 
  do 
  cs' <- foldM process oldCs l
  return $ Just cs'
  where 
    process :: Binary b => 
               CrawlerState b -> (String, Maybe (Document b), S.Set URI) -> IO (CrawlerState b)
    process cs (theMD5, mdoc, refs) 
      = if isJust mdoc
          then 
               do
               if M.member theMD5 (cs_docHashes cs)
                 then do
                      old  <- M.lookup theMD5 (cs_docHashes cs)
                      new  <- return $ uri $ fromJust mdoc
                      (newDocs, newHashes) <- update (cs_docs cs) (cs_docHashes cs) theMD5 old new
                      return cs { cs_docs = newDocs 
                                , cs_toBeProcessed = S.union 
                                       (cs_toBeProcessed cs) 
                                       (S.difference (S.filter (cs_fCrawlFilter cs) refs) 
                                                     (cs_wereProcessed cs)
                                       )
                                , cs_docHashes = newHashes
                                }
                 else do
                      return $ 
                        cs { cs_toBeProcessed = S.union (cs_toBeProcessed cs) 
                               (S.difference (S.filter (cs_fCrawlFilter cs) refs) 
                                             (cs_wereProcessed cs)
                               )
                           , cs_docs      = snd (insertDoc (cs_docs cs) (fromJust mdoc))                                                
                           , cs_docHashes = M.insert theMD5 (uri $ fromJust mdoc) (cs_docHashes cs)
                           }
          else return cs
    update :: Binary b => Documents b -> M.Map String URI -> String -> URI -> URI -> IO (Documents b, M.Map String URI)
    update docs hashes md5String oldUri newUri 
      = if length oldUri <= length newUri 
          then return $ (docs, hashes)
          else do 
               docId  <- lookupByURI docs oldUri
               oldDoc <- lookupById  docs docId
               newDoc <- return oldDoc {uri = newUri}
               return (updateDoc docs docId newDoc, M.insert md5String newUri hashes)

-- | Wrapper function for the "real" crawlDoc functions. The current time is
--   traced (to identify documents that take a lot of time to be processed while
--   testing) and the crawlDoc'-arrow is run
crawlDoc :: (Binary b) => 
            Int 
         -> CrawlerState b
         -> DocId 
         -> String 
         -> IO [(Int, (String, Maybe (Document b), S.Set URI))]
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
                 &&& crawlDoc' traceLevel attrs tmpPath refXPaths getCustom (docId, theUri) 
               )
           >>^ (\(i, (h, d, u)) -> (i, (h, d, S.fromList u)))       
          ) 

-- | Download & read document and compute document title and included refs to
--   other documents 
crawlDoc' :: (Binary b) =>
     Int
  -> Attributes    -- ^ options for readDocument
  -> Maybe String  -- ^ path for serialized tempfiles
  -> [String]
  -> Custom b
  -> (DocId, URI)   -- ^ DocId, URI
  -> IOSArrow c (String, Maybe (Document b), [URI])
crawlDoc' traceLevel attrs tmpPath refXPaths getCustom (docId, theUri) =
        traceMsg 1 ("  crawling document: " ++ show docId ++ " -> " ++ show theUri )
    >>> withTraceLevel (traceLevel - traceOffset) (readDocument attrs theUri)
    >>> (
          documentStatusOk `guards`   -- make sure that the document could be accessed
                -- if it is set in the crawler options, write a temporary copy of the document to
                -- the hard disk
          (     ( withTraceLevel (traceLevel - traceOffset)
                  (writeDocument [(a_indent, "1")] ((fromJust tmpPath) ++ (tmpFile docId theUri)))
                  `whenP`
                  const (isJust tmpPath ) 
                ) 
                -- compute a pair of Document b (Holumbus datatype) and a list of contained links
            >>> 
                (     (  xshow (this) >>^ (show . md5 . pack) )
                  &&& getDocument theUri getCustom
                  &&& ( getRefs refXPaths >>> strictA >>> perform (theTrace $< arr length) )
                )   >>^ (\(a,(b,c)) -> (a,b,c))
          )
          `orElse` (                  -- if an error occurs with the current document, the global 
                clearErrStatus        -- error status has to be reset, else the crawler would stop
            >>> traceMsg 0 (  "something went wrong with doc: \"" ++ theUri ++"\"")    
            >>> constA (show ( md5 (pack "foo")) , Nothing, [])  -- Nothing indicates the error, the empty list shows
          )                           -- that - caused by the error - no new links were found
        )
        where 
          theTrace i = traceMsg 2 ("found " ++ (show i) ++ " references")
          

-- | extract the Title of a Document (for web pages the <title> tag) and combine
--   it with the document uri
getDocument :: (Binary b) => 
               URI        -- ^ URI of the 'Document' 
            -> Custom b   -- ^ Function to extract the custom Data from the 'Document'
            -> IOSArrow XmlTree (Maybe (Document b))
getDocument theUri getCustom 
  = mkDoc $<<< (      -- get the document title. to avoid failure of the arrow if the title tag is
                      -- absent, a default value is supplied
                      ( withDefault (getXPathTrees "/html/head/title/text()" >>> getText) "")  
                 &&& constA theUri
                 &&& getCustom
               )
    where
    mkDoc t u c = constA $ Just $ Document t u c


-- | Extract References to other documents from a XmlTree based on configured XPath expressions
getRefs :: ArrowXml a => [String] -> a XmlTree [URI]
getRefs xpaths
  = listA (getRefs' $< computeDocBase) -- >>^ concat
    where
    getRefs' base = catA $ map (\x -> getXPathTrees x >>> getText >>^ toAbsRef) xpaths
      where
      toAbsRef ref = removeFragment $ fromMaybe ref $ expandURIString ref base
      removeFragment r
              | "#" `isPrefixOf` path = reverse . tail $ path
              | otherwise = r
              where
                path = dropWhile (/='#') . reverse $ r  
                
-- | create an initial CrawlerState from an IndexerConfig
initialCrawlerState :: (Binary b) => IndexerConfig -> Custom b -> CrawlerState b
initialCrawlerState cic getCustom
  = CrawlerState
    { cs_toBeProcessed  = S.fromList (ic_startPages cic)
    , cs_wereProcessed  = S.empty
    , cs_unusedDocIds   = [1..]
    , cs_readAttributes = ic_readAttributes cic
    , cs_refXPaths      = ["//a/@href/text()", "//frame/@src/text()", "//iframe/@src/text()"]
    , cs_fPreFilter     = this
    , cs_fCrawlFilter   = ic_fCrawlFilter cic
    , cs_docs           = emptyDocuments
    , cs_tempPath       = ic_tmpPath cic
    , cs_fGetCustom     = getCustom
    , cs_docHashes      = M.empty
    }
    
    
saveCrawlerState :: Binary a => FilePath -> CrawlerState a -> IO ()
saveCrawlerState fp cs = writeToBinFile fp cs

loadCrawlerState :: Binary a => FilePath -> CrawlerState a -> IO (CrawlerState a)
loadCrawlerState fp ori = do
                          cs <- decodeFile fp
                          return $! cs { cs_readAttributes = cs_readAttributes ori
                                    , cs_fPreFilter     = cs_fPreFilter     ori
                                    , cs_refXPaths      = cs_refXPaths      ori
                                    , cs_tempPath       = cs_tempPath       ori
                                    , cs_fGetCustom     = cs_fGetCustom     ori
                                    }    
          


         