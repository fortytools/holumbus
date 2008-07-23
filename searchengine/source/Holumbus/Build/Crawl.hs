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
  , crawlFileSystem
  
  -- * Initializing
  , initialCrawlerState
--  , getRefs
  
  -- * Persistence
  , loadCrawlerState
  , saveCrawlerState
  )
where

import           Control.Monad hiding (when)

import           Data.Binary
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Map    as M
import qualified Data.Set    as S
-- import qualified Data.IntMap as IM
import           Data.Digest.MD5
import           Data.ByteString.Lazy.Char8(pack)

import           Holumbus.Build.Config
import           Holumbus.Control.MapReduce.ParallelWithClass -- Persistent
import           Holumbus.Control.MapReduce.MapReducible

import           Holumbus.Index.Common
import           Holumbus.Utility

import           Text.XML.HXT.Arrow hiding (getXPathTrees)
import           Text.XML.HXT.Arrow.XPathSimple
import           System.Directory
import           System.Time

-- import Control.Parallel.Strategies

type MD5Hash = String


instance (Binary a, HolDocuments d a) => MapReducible (CrawlerState d a) () (String, Maybe (Document a), S.Set URI)
  where 
  reduceMR     = processCrawlResults
  mergeMR _ cs = return cs
         
crawlFileSystem :: HolDocuments d Int =>  [FilePath] -> (FilePath -> Bool) -> d Int -> IO (d Int)
crawlFileSystem startPages docFilter emptyDocuments 
  = do
    docs <- mapM (crawlFileSystem' docFilter) startPages
    foldM  (\ds s -> return $ snd $ insertDoc ds (Document "" s (Nothing :: Maybe Int))) emptyDocuments (concat docs)

crawlFileSystem' :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
crawlFileSystem' docFilter path
  = do
    dc     <- getDirectoryContents path
    absdc  <- return $ map (\s -> path ++ "/" ++ s) (filter (\s ->  (s /= ".") && (s /= "..")) dc)
    foldM process [] (filter (\s -> (docFilter s)) absdc)
    where
      process :: [FilePath] -> FilePath -> IO [FilePath]
      process res s = do
                      exists <- doesDirectoryExist s
                      if exists 
                        then do
                             rec <- crawlFileSystem' docFilter s
                             return $ res ++ rec
                        else do
                             print s
                             return $ s : res

-- | The crawl function. MapReduce is used to scan the documents for references to other documents
--   that have to be added to the 'Documents', too.
crawl :: (Show a, HolDocuments d a, Binary a, Binary (d a), XmlPickler (d a)) => Int -> Int -> Int -> CrawlerState d a -> IO (d a)
crawl traceLevel maxWorkers maxDocs cs = 
  if S.null ( cs_toBeProcessed cs ) -- if no more documents have to be processed, 
    then return (cs_docs cs)        -- the Documents are returned
    else do                         -- otherwise, the crawling is continued
                 -- get the Docs that have to be processed and add docIds
                 -- for the MapReduce Computation
         d    <- return $ if maxDocs == 0 
                            then cs_toBeProcessed cs
                            else (S.fromList . (take maxDocs) . S.toList) (cs_toBeProcessed cs)
         d'   <- return $ zip [(cs_nextDocId cs)..] (S.toList d) -- (S.toList $ cs_toBeProcessed cs)
         
--         saveCrawlerState ( (fromMaybe "/tmp/" (cs_tempPath cs) ) ++ "CrawlerState.bin") cs
--         writeToXmlFile   ( (fromMaybe "/tmp/" (cs_tempPath cs) ) ++ "Docs.xml") (cs_docs cs)
                                      
         runX (traceMsg 0 ("          Status: already processed: " ++ show (S.size $ cs_wereProcessed cs) ++ 
                           ", to be processed: "   ++ show (S.size $ cs_toBeProcessed cs)))
         
         cs'  <- return $ cs { cs_nextDocId = cs_nextDocId cs + length d'
                             , cs_wereProcessed = S.union d (cs_wereProcessed cs)
                             , cs_toBeProcessed = S.difference (cs_toBeProcessed cs) d
                             }

         let attrs     = cs_readAttributes cs 
             tmpPath   = cs_tempPath cs
             getRefs   = cs_fGetReferences cs
             getCustom = cs_fGetCustom cs 
         
         csnew   <- mapReduce maxWorkers 
--                              (cs_crawlerTimeOut cs) 
--                              (fromMaybe "/tmp/" (cs_tempPath cs)  ++ "crawl") 
                              cs' 
                              (crawlDoc traceLevel attrs tmpPath getRefs getCustom)
                              d' 
                               
         crawl traceLevel maxWorkers maxDocs csnew -- (snd $ M.elemAt 0 mr)


-- | The REDUCE function for the crawling MapReduce computation. The 'Documents' and their contained
--   links are used to modify the 'CrawlerState'. New documents are added to the list of 
--   unprocessed docs and the data of the already crawled documents are added to the 'Documents'. 
processCrawlResults :: (HolDocuments d a, Binary a) => 
                       CrawlerState d a                           -- ^ state before last MapReduce job
                    -> ()                                         -- ^
                    -> [(MD5Hash, Maybe (Document a), S.Set URI)]  -- ^ data produced in the crawl phase
                    -> IO (Maybe (CrawlerState d a))
processCrawlResults oldCs _ l = 
  do 
  cs' <- foldM process oldCs l
  return $ Just cs'
  where 
    process :: (HolDocuments d a, Binary a) => 
               CrawlerState d a -> (String, Maybe (Document a), S.Set URI) -> IO (CrawlerState d a)
    process cs (theMD5, mdoc, refs) 
      = if isJust mdoc
          then 
               do
               if isJust (cs_docHashes cs) && M.member theMD5 (fromJust $ cs_docHashes cs)
                 then do
                      let hashes = fromJust $ cs_docHashes cs
                      old  <- M.lookup theMD5 hashes
                      new  <- return $ uri $ fromJust mdoc
                      (newDocs, newHashes) <- update (cs_docs cs) hashes theMD5 old new
                      return cs { cs_docs = newDocs 
                                , cs_toBeProcessed = S.union 
                                       (cs_toBeProcessed cs) 
                                       (S.difference (S.filter (cs_fCrawlFilter cs) refs) 
                                                     (cs_wereProcessed cs)
                                       )
                                , cs_docHashes = Just $ newHashes
                                }
                 else do
                      return $ 
                        cs { cs_toBeProcessed = S.union (cs_toBeProcessed cs) 
                               (S.difference (S.filter (cs_fCrawlFilter cs) refs) 
                                             (cs_wereProcessed cs)
                               )
                           , cs_docs      = snd (insertDoc (cs_docs cs) (fromJust mdoc))                                                
                           , cs_docHashes = if isJust (cs_docHashes cs)
                                              then Just $ M.insert theMD5 (uri $ fromJust mdoc) (fromJust $ cs_docHashes cs)
                                              else Nothing
                           }
          else return cs
    update :: (HolDocuments d a, Binary a) => 
              d a -> M.Map String URI -> String -> URI -> URI -> IO (d a, M.Map String URI)
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
crawlDoc :: (Binary a, Show a) => 
            Int 
         -> Attributes
         -> Maybe String
         -> IOSArrow XmlTree [URI]
         -> Custom a
         -> DocId 
         -> String 
         -> IO [((), (MD5Hash, Maybe (Document a), S.Set URI))]
crawlDoc traceLevel attrs tmpPath getRefs getCustom docId theUri 
  = do
    clt <- getClockTime               -- get Clock Time for debugging
    cat <- toCalendarTime clt         -- and convert it to "real" date and time
    r <- runX (     setTraceLevel traceLevel
               >>> traceMsg 1 (calendarTimeToString cat)  
               >>> (     constA ()        -- this is needed to fit the MapReduce abstraction
                     &&& crawlDoc' traceLevel attrs tmpPath getRefs getCustom (docId, theUri) 
                   )
               >>^ (\(i, (h, d, u)) -> (i, (h, d, S.fromList u)))       
              )
--    putStrLn (show r)
    return $ r 

-- | Download & read document and compute document title and included refs to
--   other documents 
crawlDoc' :: (Binary a) =>
     Int
  -> Attributes    -- ^ options for readDocument
  -> Maybe String  -- ^ path for serialized tempfiles
  -> IOSArrow XmlTree [URI]
--  -> [String]
  -> Custom a
  -> (DocId, URI)   -- ^ DocId, URI
  -> IOSArrow c (String, Maybe (Document a), [URI])
crawlDoc' traceLevel attrs tmpPath getRefs getCustom (docId, theUri) =
        traceMsg 1 ("  crawling document: " ++ show docId ++ " -> " ++ show theUri )
    >>> withTraceLevel (traceLevel - traceOffset) (readDocument attrs theUri)
    >>> (
          documentStatusOk `guards`   -- make sure that the document could be accessed
                -- if it is set in the crawler options, write a temporary copy of the document to
                -- the hard disk
          (     ( if isJust tmpPath
		  then withTraceLevel (traceLevel - traceOffset) $
                       writeDocument standardWriteTmpDocumentAttributes
                                     (fromJust tmpPath ++ tmpFile docId theUri)
		  else this
                ) 
                -- compute a pair of Document b (Holumbus datatype) and a list of contained links
            >>> 
                (     (  xshow (this) >>^ (show . md5 . pack) )
                  &&& getDocument theUri getCustom
                  &&& ( getRefs >>> strictA >>> perform (theTrace $< arr length) )
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


         