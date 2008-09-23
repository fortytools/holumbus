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

module Examples.Crawler.Crawl
  (
  -- * CrawlerState type
    CrawlerState (..)

  , crawlerAction  

  -- * Crawling
  , crawl
  
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
-- import           Holumbus.Control.MapReduce.ParallelWithClass -- Persistent
-- import           Holumbus.Control.MapReduce.MapReducible

import           Holumbus.Index.Common
import           Holumbus.Utility

import           Text.XML.HXT.Arrow hiding (getXPathTrees)
import           Text.XML.HXT.Arrow.XPathSimple
-- import           System.Directory
import           System.Time

import qualified Holumbus.FileSystem.FileSystem as FS
import           Holumbus.MapReduce.Types
import           Holumbus.MapReduce.MapReduce

import           Examples.Crawler.Config

-- import Control.Parallel.Strategies

type MD5Hash = String


-- because of IO(Maybe v3) --> has to instanciate the typeclass show
instance Show (CrawlerState d a) where
  show _ = "CrawlerState"

-- ----------------------------------------------------------------------------
-- Map-Action
-- ----------------------------------------------------------------------------


crawlerAction
  :: (Binary a, Show a, HolDocuments d a)
  => MRCrawlerConfig d a                           -- whitness and configuration
  -> ActionConfiguration 
       (Int, CrawlerState d a)                     -- state
       DocId String                                -- k1, v1
       () (MD5Hash, Maybe (Document a), S.Set URI) -- k2, v2
       (MD5Hash, Maybe (Document a), S.Set URI)    -- v3 == v2
       (CrawlerState d a)                          -- v4
crawlerAction cc
  = (defaultActionConfiguration "CRAWL")
        { ac_Map     = Just mapAction
        , ac_Combine = Nothing
        , ac_Reduce  = Just reduceAction
        }
    where
      mapAction 
        = (defaultMapConfiguration (mapCrawl cc))           
      reduceAction
        = (defaultReduceConfiguration (reduceCrawl cc))
            { rc_Merge     = mergeCrawl }


mapCrawl
  :: (Binary a, Show a, HolDocuments d a)
  => MRCrawlerConfig d a                 -- ^ static configuration
  -> ActionEnvironment
  -> (Int, CrawlerState d a)             -- ^ state
  -> DocId -> String                     -- ^ key - value
  -> IO [((), (MD5Hash, Maybe (Document a), S.Set URI))]
mapCrawl cc env (traceLevel,_) docId theUri
  = do
    let cs         = cc_CrawlerState cc
        attrs      = cs_readAttributes cs 
        tmpPath    = cs_tempPath cs
        getRefs    = cs_fGetReferences cs
        getCustom  = cs_fGetCustom cs
        fileSystem = ae_FileSystem env

    runX (traceMsg 0 ("DocId: " ++ show docId ++ " - Uri: " ++ show theUri))
    -- putStrLn "################"
    -- putStrLn $ show attrs
    -- putStrLn "################"
    res <- crawlDoc traceLevel fileSystem attrs tmpPath getRefs getCustom docId theUri
    runX (traceMsg 0 ("Result: "))
    runX (traceMsg 0 (show res))
    return res


-- ----------------------------------------------------------------------------
-- Reduce-Action
-- ----------------------------------------------------------------------------

mergeCrawl
  :: (Binary a, Show a, HolDocuments d a)
  -- => CrawlerState d a         -- ^ type Whitness
  => ActionEnvironment
  -> (Int,CrawlerState d a)   -- ^ state
  -> [((), (MD5Hash, Maybe (Document a), S.Set URI))]
  -> IO [((), [(MD5Hash, Maybe (Document a), S.Set URI)])]
mergeCrawl _ _ ls = return [((), ls')]
  where
  ls' = foldr (\(_,v) vs -> v:vs) [] ls


reduceCrawl
  :: (Binary a, Show a, HolDocuments d a)
  => MRCrawlerConfig d a          -- ^ type Whitness 
  -> ActionEnvironment
  -> (Int,CrawlerState d a)    -- ^ state
  -> () -> [(MD5Hash, Maybe (Document a), S.Set URI)]
  -> IO (Maybe (CrawlerState d a))
reduceCrawl cc _ (_,cs) k ls = processCrawlResults cs' cs k ls
  where
  cs' = cc_CrawlerState cc



-- | The crawl function. MapReduce is used to scan the documents for references to other documents
--   that have to be added to the 'Documents', too.
crawl
 :: (Show a, HolDocuments d a, Binary a, Binary (d a), XmlPickler (d a)
    , MapReduce mr) 
 => MRCrawlerConfig d a                           -- whitness and configuration
 -> Int -> Int
 -> CrawlerState d a
 -> mr
 -> IO (d a)
crawl config traceLevel maxDocs cs mr = 
  if S.null ( cs_toBeProcessed cs ) -- if no more documents have to be processed, 
    then do
      runX (traceMsg 0 (" no more documents to processed"))
      return (cs_docs cs)        -- the Documents are returned
    else do                         -- otherwise, the crawling is continued
       runX (traceMsg 0 (" new crawler-cycle"))
       -- get the Docs that have to be processed and add docIds
       -- for the MapReduce Computation
       d    <- return $ if maxDocs == 0 
                          then cs_toBeProcessed cs
                          else (S.fromList . (take maxDocs) . S.toList) (cs_toBeProcessed cs)
       d'   <- return $ zip [(cs_nextDocId cs)..] (S.toList d) -- (S.toList $ cs_toBeProcessed cs)
       runX (traceMsg 0 (" next docs to be pulled:"))
       runX (traceMsg 0 (show d'))
       
       -- saveCrawlerState ( (fromMaybe "/tmp/" (cs_tempPath cs) ) ++ "CrawlerState.bin") cs
       -- writeToXmlFile   ( (fromMaybe "/tmp/" (cs_tempPath cs) ) ++ "Docs.xml") (cs_docs cs)
                                    
       runX (traceMsg 0 ("          Status: already processed: " ++ show (S.size $ cs_wereProcessed cs) ++ 
                         ", to be processed: "   ++ show (S.size $ cs_toBeProcessed cs)))
       
       cs'  <- return $ cs { cs_nextDocId = cs_nextDocId cs + length d'
                           , cs_wereProcessed = S.union d (cs_wereProcessed cs)
                           , cs_toBeProcessed = S.difference (cs_toBeProcessed cs) d
                           }

       (res,_) <- doMapReduce (crawlerAction config) (traceLevel,cs') d' [] 1 5 1 1 TOTRawTuple mr
       
       runX (traceMsg 0 (" result of this cycle: "))       
       
       let (_,csnew) = head res
       
       runX (traceMsg 0 (show $ cs_toBeProcessed csnew))
                             
       crawl config traceLevel maxDocs csnew mr


-- | The REDUCE function for the crawling MapReduce computation. The 'Documents' and their contained
--   links are used to modify the 'CrawlerState'. New documents are added to the list of 
--   unprocessed docs and the data of the already crawled documents are added to the 'Documents'. 
processCrawlResults :: (HolDocuments d a, Binary a) => 
                       CrawlerState d a
                    -> CrawlerState d a                           -- ^ state before last MapReduce job
                    -> ()                                         -- ^
                    -> [(MD5Hash, Maybe (Document a), S.Set URI)] -- ^ data produced in the crawl phase
                    -> IO (Maybe (CrawlerState d a))
processCrawlResults cc oldCs _ l = 
  do 
  cs' <- foldM process oldCs l
  runX (traceMsg 0 (" ----------------------"))
  runX (traceMsg 0 (" old cs: "))
  runX (traceMsg 0 (show $ cs_toBeProcessed oldCs))
  runX (traceMsg 0 (" ----------------------"))
  runX (traceMsg 0 (" new cs: "))
  runX (traceMsg 0 (show $ cs_toBeProcessed cs'))
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
                                       (S.difference (S.filter (cs_fCrawlFilter cc) refs) 
                                                     (cs_wereProcessed cs)
                                       )
                                , cs_docHashes = Just $ newHashes
                                }
                 else do
                      return $ 
                        cs { cs_toBeProcessed = S.union (cs_toBeProcessed cs) 
                               (S.difference (S.filter (cs_fCrawlFilter cc) refs) 
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
         -> FS.FileSystem
         -> Attributes
         -> Maybe String
         -> IOSArrow XmlTree [URI]
         -> Custom a
         -> DocId 
         -> String 
         -> IO [((), (MD5Hash, Maybe (Document a), S.Set URI))]
crawlDoc traceLevel fileSystem attrs tmpPath getRefs getCustom docId theUri 
  = do
    clt <- getClockTime               -- get Clock Time for debugging
    cat <- toCalendarTime clt         -- and convert it to "real" date and time
    r <- runX (     setTraceLevel traceLevel
               >>> traceMsg 1 (calendarTimeToString cat)  
               >>> (     constA ()        -- this is needed to fit the MapReduce abstraction
                     &&& crawlDoc' traceLevel fileSystem attrs tmpPath getRefs getCustom (docId, theUri) 
                   )
               >>^ (\(i, (h, d, u)) -> (i, (h, d, S.fromList u)))       
              )
--    putStrLn (show r)
    return $ r 

-- | Download & read document and compute document title and included refs to
--   other documents 
crawlDoc' :: (Binary a) =>
     Int
  -> FS.FileSystem
  -> Attributes    -- ^ options for readDocument
  -> Maybe String  -- ^ path for serialized tempfiles
  -> IOSArrow XmlTree [URI]
--  -> [String]
  -> Custom a
  -> (DocId, URI)   -- ^ DocId, URI
  -> IOSArrow c (String, Maybe (Document a), [URI])
crawlDoc' traceLevel fileSystem attrs tmpPath getRefs getCustom (docId, theUri) =
        traceMsg 1 ("  crawling document: " ++ show docId ++ " -> " ++ show theUri )
    >>> withTraceLevel (traceLevel - traceOffset) (readDocument attrs theUri)
    >>> (
          documentStatusOk `guards`   -- make sure that the document could be accessed
                -- if it is set in the crawler options, write a temporary copy of the document to
                -- the hard disk
          (     ( if isJust tmpPath
          then withTraceLevel (traceLevel - traceOffset) $
                        (replaceBaseElement $< computeDocBase)    -- make sure that there ist a <base> tag
                        -- a b c >>> a c d -> a b d
                        >>> -- writeDocument :: Attributes -> String -> IOStateArrow s XmlTree XmlTree
                            -- writeDocument standardWriteTmpDocumentAttributes
                            --         (fromJust tmpPath ++ tmpFile docId theUri)
                        
                            -- writeDocumentToString :: Attributes -> IOStateArrow s XmlTree String      
                            (writeDocumentToString standardWriteTmpDocumentAttributes
                             >>> 
                             --   arrIO :: (b -> IO c) -> a b c
                             arrIO (\s -> FS.createFile 
                                           ({- fromJust tmpPath ++ -} tmpFile docId theUri) 
                                           (encode s)
                                           fileSystem))
                             &&& (returnA)
                          >>> arr (\(_,t) -> t)
                        --IOStateArrow s XmlTree XmlTree
                        -- arr           b       c          arr          c      d      arr          b       d
                        --IOStateArrow s XmlTree String >>> IOStateArrow String () >>> IOSTateArrow XmlTree () 
                        --arr b c >>> arr c d >> arr b c
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
                traceMsg 0 (  "something went wrong with doc: \"" ++ theUri ++"\"")    
            >>> clearErrStatus        -- error status has to be reset, else the crawler would stop
            >>> constA (show ( md5 (pack "foo")) , Nothing, [])  -- Nothing indicates the error, the empty list shows
          )                           -- that - caused by the error - no new links were found
        )
        where 
          theTrace i = traceMsg 2 ("found " ++ (show i) ++ " references")


-- | Replaces the <base> tag in the Document. If there is no <base> tag, a new one is inserted.
-- This is especially usefull when saving the document to a local file. With a <base> tag
-- links in the document can be properly resolved.
replaceBaseElement :: ArrowXml a => String -> a XmlTree XmlTree
replaceBaseElement base = processTopDownUntil ( (isElem >>> hasName "html") `guards` processHtml )
    where
    processHtml = processTopDownUntil ( (isElem >>> hasName "head") `guards` processHead)
    processHead = ifA (getChildren >>> isElem >>> hasName "base") 
                        (processTopDownUntil ( (isElem >>> hasName "base") `guards` processAttr) )
                        addBaseElem
    processAttr = processAttrl ( changeAttrValue (\_ -> base ) `when` hasName "href")
    addBaseElem = replaceChildren (getChildren <+> aelem "base" [sattr "href" base]) 
          

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


         
