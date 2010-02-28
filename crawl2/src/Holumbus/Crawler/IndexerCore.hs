{-# OPTIONS -XFlexibleContexts -XBangPatterns #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.IndexerCore
    ( RawDoc
    , RawContexts
    , RawContext
    , RawWords
    , RawWord
    , RawTitle
    , IndexCrawlerConfig
    , IndexContextConfig(..)
    , IndexerState(..)
    , emptyIndexerState
    , indexCrawlerConfig
    , stdIndexer
    )
where

-- ------------------------------------------------------------

import           Control.DeepSeq
import           Control.Monad                  ( foldM )
import           Control.Monad.Trans            ( MonadIO )

import           Data.Binary                    ( Binary )
import qualified Data.Binary                    as B                            -- else naming conflict with put and get from Monad.State
import           Data.Function.Selector
-- import           Data.List
import           Data.Maybe

import           Holumbus.Crawler.Constants
import           Holumbus.Crawler.Core
import           Holumbus.Crawler.Html
import           Holumbus.Crawler.URIs

import           Holumbus.Index.Common          hiding ( URI )

import           Text.XML.HXT.Arrow

-- ------------------------------------------------------------

type RawDoc c                   = (RawContexts, RawTitle, Maybe c)              -- c is the user defined custom info
type RawContexts                = [RawContext]
type RawContext                 = (Context, RawWords)
type RawWords                   = [RawWord]
type RawWord                    = (Word, Position)
type RawTitle                   = String

type IndexCrawlerConfig i d c   = CrawlerConfig (RawDoc c) (IndexerState i d c)

data IndexContextConfig         = IndexContextConfig
                                  { ixc_name           :: String
                                  , ixc_collectText    :: IOSArrow XmlTree String
                                  , ixc_textToWords    :: String -> [String]
                                  , ixc_boringWord     :: String -> Bool
                                  }   

data IndexerState i d c         = IndexerState
                                  { ixs_index           :: ! i                  -- the index type
                                  , ixs_documents       :: ! (d c)              -- the user defined type for document descriptions
                                  } deriving (Show)

-- ------------------------------------------------------------

instance (NFData i, NFData (d c)) => NFData (IndexerState i d c) where
    rnf IndexerState { ixs_index = i, ixs_documents = d }
                                = rnf i `seq` rnf d

-- ------------------------------------------------------------

instance (Binary i, Binary (d c)) => Binary (IndexerState i d c) where
    put s               = B.put (ixs_index s)
                          >>
                          B.put (ixs_documents s)
    get                 = do
                          ix <- B.get
                          dm <- B.get
                          return $ IndexerState
                                   { ixs_index          = ix
                                   , ixs_documents      = dm
                                   }

instance (XmlPickler i, XmlPickler (d c)) => XmlPickler (IndexerState i d c) where
    xpickle             = xpElem "index-state" $
                          xpWrap (uncurry IndexerState, \ ix -> (ixs_index ix, ixs_documents ix)) $
                          xpPair xpickle xpickle

-- ------------------------------------------------------------

emptyIndexerState               :: i -> d c -> IndexerState i d c
emptyIndexerState eix edm       = IndexerState
                                  { ixs_index           = eix
                                  , ixs_documents       = edm
                                  }

-- ------------------------------------------------------------

stdIndexer                      :: (Binary i, HolIndexM IO i, HolDocuments d c, NFData i, NFData (d c), NFData c, Binary c) =>
                                   Maybe String                                 -- ^ resume from interrupted index run with state stored in file
                                -> [URI]                                        -- ^ start indexing with this set of uris
                                -> IndexCrawlerConfig i d c                     -- ^ adapt configuration to special needs, use id if default is ok
                                -> IndexerState i d c                           -- ^ the initial empty indexer state
                                -> IO (IndexerState i d c)                      -- ^ result is a state consisting of the index and the map of indexed documents

stdIndexer resumeLoc startUris config eis
                                = do
                                  (_, ixState) <- runCrawler action config (initCrawlerState eis)
                                  return (getS theResultAccu ixState)
    where
    action                      = do
                                  noticeC "indexerCore" ["indexer started"]
                                  res <- maybe (crawlDocs startUris) crawlerResume $ resumeLoc
                                  noticeC "indexerCore" ["indexer finished"]
                                  return res

-- ------------------------------------------------------------

-- general HolIndexM IO i version, for old specialized version see code at end of this file

indexCrawlerConfig              :: (HolIndexM IO i, HolDocuments d c, NFData i, NFData c) =>
                                   Attributes                                   -- ^ document read options
                                -> (URI -> Bool)                                -- ^ the filter for deciding, whether the URI shall be processed
                                -> Maybe (IOSArrow XmlTree String)              -- ^ the document href collection filter, default is 'Holumbus.Crawler.Html.getHtmlReferences'
                                -> Maybe (IOSArrow XmlTree XmlTree)             -- ^ the pre document filter, default is the this arrow
                                -> Maybe (IOSArrow XmlTree String)              -- ^ the filter for computing the document title, default is empty string
                                -> Maybe (IOSArrow XmlTree c)                   -- ^ the filter for the cutomized doc info, default Nothing
                                -> [IndexContextConfig]                         -- ^ the configuration of the various index parts
                                -> IndexCrawlerConfig i d c                     -- ^ result is a crawler config

indexCrawlerConfig opts followRef getHrefF preDocF titleF0 customF0 contextCs
                                = addReadAttributes defaultOpts                 -- install the default read options
                                  >>>
                                  addReadAttributes opts                        -- overwrite and add specific read options
                                  >>>
                                  ( setS theFollowRef followRef )
                                  >>>
                                  ( setS theProcessRefs   $ fromMaybe getHtmlReferences getHrefF )
                                  >>>
                                  ( setS thePreDocFilter  $ fromMaybe checkDocumentStatus preDocF )     -- in case of errors throw away any contents
                                  >>>
                                  ( setS theProcessDoc rawDocF )                -- rawDocF is build up by the context config, text, title and custom
                                  >>>
                                  enableRobotsTxt                               -- add the robots stuff at the end
                                  >>>                                           -- the filter wrap the other filters
                                  addRobotsNoFollow
                                  >>>
                                  addRobotsNoIndex
                                  $
                                  defaultCrawlerConfig insertRawDocM unionIndexerStatesM
									        -- take the default crawler config
                                                                                -- and set the result combining functions
    where
    rawDocF                     = ( listA contextFs
                                    &&&
                                    titleF
                                    &&&
                                    customF
                                  )
                                  >>^ (\ (x3, (x2, x1)) -> (x3, x2, x1))

    titleF                      = ( fromMaybe (constA "") titleF0 ) >. concat

    customF                     = ( fromMaybe none customF0 ) >. listToMaybe
                                  
    contextFs                   :: IOSArrow XmlTree RawContext
    contextFs                   = catA . map contextF $ contextCs               -- collect all contexts

    contextF                    :: IndexContextConfig -> IOSArrow XmlTree RawContext
    contextF ixc                = constA (ixc_name ixc)                         -- the name of the raw context
                                  &&&
                                  ( ixc_collectText ixc >. processText )        -- the list of words and positions of the collected text
        where                                                                   -- this arrow is deterministic, it always delivers a single pair
        processText             :: [String] -> RawWords
        processText             = concat
                                  >>>
                                  ixc_textToWords ixc
                                  >>>
                                  flip zip [1..]
                                  >>>
                                  filter (fst >>> ixc_boringWord ixc >>> not)

    defaultOpts                 = [ (curl_max_filesize,         "1000000")      -- limit document size to 1 Mbyte
                                  , (curl_location,             v_1)            -- automatically follow redirects
                                  , (curl_max_redirects,        "3")            -- but limit # of redirects to 3
                                  , (a_accept_mimetypes,        "text/html text/xhtml")
                                  , (a_encoding,                isoLatin1)
                                  , (a_ignore_encoding_errors,  v_1)            -- encoding errors and parser warnings are boring
                                  , (a_validate,                v_0)
                                  , (a_parse_html,              v_1)
                                  , (a_issue_warnings,  	v_0)
                                  ]

-- ------------------------------------------------------------

unionIndexerStatesM		:: (MonadIO m, HolIndexM m i, HolDocuments d c) =>
                                   IndexerState i d c
                                -> IndexerState i d c
                                -> m (IndexerState i d c)
unionIndexerStatesM ixs1 ixs2
    | s1 < s2			= unionIndexerStatesM ixs2 ixs1
    | otherwise			= do
                                  ix2s <- updateDocIdsM' (+ m1) ix2
                                  ix   <- mergeIndexesM ix1 ix2s
                                  return $!
                                         IndexerState { ixs_index        = ix -- mergeIndexes ix1 ix2s
					              , ixs_documents    = unionDocs    dt1 dt2s
					              }
    where
    ix1				= ixs_index     ixs1
    ix2				= ixs_index     ixs2
    dt1				= ixs_documents ixs1
    dt2				= ixs_documents ixs2
    dt2s			= editDocIds    (+ m1) dt2
    s1				= sizeDocs dt1
    s2				= sizeDocs dt2
    m1				= maxDocId dt1

-- ------------------------------------------------------------

insertRawDocM                   :: (MonadIO m, HolIndexM m i, HolDocuments d c, NFData i, NFData c) =>
                                   (URI, RawDoc c)                              -- ^ extracted URI and doc info
                                -> IndexerState i d c                           -- ^ old indexer state
                                -> m (IndexerState i d c)                       -- ^ new indexer state

insertRawDocM (!rawUri, (!rawContexts, !rawTitle, !rawCustom)) ixs
                                = do
                                  newIx <- foldM (insertRawContextM did) (ixs_index ixs) $
                                           (rnf rawContexts `seq` rawContexts)
				  rnf newIx `seq` rnf doc `seq`
                                    return $! IndexerState { ixs_index        = newIx
							   , ixs_documents    = newDocs
							   }
    where
    (!did, !newDocs)            = insertDoc (ixs_documents ixs) doc
    !doc                        = Document
                                  { title       = rawTitle
                                  , uri         = rawUri
                                  , custom      = rawCustom
                                  }

insertRawContextM             	:: (Monad m, HolIndexM m i) =>
                                   DocId -> i -> (Context, [(Word, Position)]) -> m i
insertRawContextM did ix (cx, ws)
				= foldM (insWordM cx did) ix ws

insWordM 			:: (Monad m, HolIndexM m i) =>
                                   Context -> DocId -> i -> (Word, Int) -> m i
insWordM cx' did' ix' (w', p')  = insertPositionM cx' w' did' p' ix'



-- ------------------------------------------------------------
-- old stuff
-- ------------------------------------------------------------

{- this code works with pure HolIndex types, not with monadic ones,
   but there is a default instance implementation

   HolIndex i => HolIndexM m i

   so this code is too specialized

-- ------------------------------------------------------------

indexCrawlerConfig              :: (HolIndex i, HolDocuments d c, NFData c) =>
                                   Attributes                                   -- ^ document read options
                                -> (URI -> Bool)                                -- ^ the filter for deciding, whether the URI shall be processed
                                -> Maybe (IOSArrow XmlTree String)              -- ^ the document href collection filter, default is 'Holumbus.Crawler.Html.getHtmlReferences'
                                -> Maybe (IOSArrow XmlTree XmlTree)             -- ^ the pre document filter, default is the this arrow
                                -> Maybe (IOSArrow XmlTree String)              -- ^ the filter for computing the document title, default is empty string
                                -> Maybe (IOSArrow XmlTree c)                   -- ^ the filter for the cutomized doc info, default Nothing
                                -> [IndexContextConfig]                         -- ^ the configuration of the various index parts
                                -> IndexCrawlerConfig i d c                     -- ^ result is a crawler config

indexCrawlerConfig opts followRef getHrefF preDocF titleF0 customF0 contextCs
                                = addReadAttributes defaultOpts                 -- install the default read options
                                  >>>
                                  addReadAttributes opts                        -- overwrite and add specific read options
                                  >>>
                                  ( setS theFollowRef followRef )
                                  >>>
                                  ( setS theProcessRefs   $ fromMaybe getHtmlReferences getHrefF )
                                  >>>
                                  ( setS thePreDocFilter  $ fromMaybe checkDocumentStatus preDocF )     -- in case of errors throw away any contents
                                  >>>
                                  ( setS theProcessDoc rawDocF )                -- rawDocF is build up by the context config, text, title and custom
                                  >>>
                                  enableRobotsTxt                               -- add the robots stuff at the end
                                  >>>                                           -- the filter wrap the other filters
                                  addRobotsNoFollow
                                  >>>
                                  addRobotsNoIndex
                                  $
                                  defaultCrawlerConfig insertRawDoc             -- take the default crawler config
                                                                                -- and set the result combining function
    where
    rawDocF                     = ( listA contextFs
                                    &&&
                                    titleF
                                    &&&
                                    customF
                                  )
                                  >>^ (\ (x3, (x2, x1)) -> (x3, x2, x1))

    titleF                      = ( fromMaybe (constA "") titleF0 ) >. concat

    customF                     = ( fromMaybe none customF0 ) >. listToMaybe
                                  
    contextFs                   :: IOSArrow XmlTree RawContext
    contextFs                   = catA . map contextF $ contextCs               -- collect all contexts

    contextF                    :: IndexContextConfig -> IOSArrow XmlTree RawContext
    contextF ixc                = constA (ixc_name ixc)                         -- the name of the raw context
                                  &&&
                                  ( ixc_collectText ixc >. processText )        -- the list of words and positions of the collected text
        where                                                                   -- this arrow is deterministic, it always delivers a single pair
        processText             :: [String] -> RawWords
        processText             = concat
                                  >>>
                                  ixc_textToWords ixc
                                  >>>
                                  flip zip [1..]
                                  >>>
                                  filter (fst >>> ixc_boringWord ixc >>> not)

    defaultOpts                 = [ (curl_max_filesize,         "1000000")      -- limit document size to 1 Mbyte
                                  , (curl_location,             v_1)            -- automatically follow redirects
                                  , (curl_max_redirects,        "3")            -- but limit # of redirects to 3
                                  , (a_accept_mimetypes,        "text/html")
                                  , (a_encoding,                isoLatin1)
                                  , (a_ignore_encoding_errors,  v_1)            -- encoding errors and parser warnings are boring
                                  , (a_validate,                v_0)
                                  , (a_parse_html,              v_1)
                                  , (a_issue_warnings,  v_0)
                                  ]

-- ------------------------------------------------------------

insertRawDoc                    :: (HolIndex i, HolDocuments d c, NFData c) =>
                                   (URI, RawDoc c)                              -- ^ extracted URI and doc info
                                -> IndexerState i d c                           -- ^ old indexer state
                                -> IO (IndexerState i d c)                      -- ^ new indexer state

insertRawDoc (rawUri, (rawContexts, rawTitle, rawCustom)) ixs
                                = return $
                                  IndexerState
                                  { ixs_index           = newIx
                                  , ixs_documents       = newDocs
                                  }
    where
    newIx                       = foldl' (insertRawContext did) (ixs_index ixs) -- insert all raw contexts
                                  $ (rawContexts `using` rnf)                   -- raw context is reduced to normal form

    (did, newDocs)              = insertDoc (ixs_documents ixs) doc             -- create new doc id and insert doc into documents tableimport

    doc                         = Document                                      -- Document is reduced to normal form
                                  { title       = rawTitle  `using` rnf
                                  , uri         = rawUri    `using` rnf
                                  , custom      = rawCustom `using` rnf
                                  }

insertRawContext                :: (HolIndex i) =>
                                   DocId -> i -> (Context, [(Word, Position)]) -> i
insertRawContext did ix (cx,ws) = M.foldWithKey insWs ix wpm
    where
    insWs w ps                  = insertOccurrences cx w (IM.singleton did ps)
    wpm                         = foldl' (flip ins) M.empty $ ws
    ins (w, p)                  = M.insertWith IS.union w (IS.singleton p)


-- ------------------------------------------------------------

stdIndexer                      :: (HolIndex i, HolDocuments d c, Binary c) =>
                                   Maybe String                                 -- ^ resume from interrupted index run with state stored in file
                                -> [URI]                                        -- ^ start indexing with this set of uris
                                -> IndexCrawlerConfig i d c                     -- ^ adapt configuration to special needs, use id if default is ok
                                -> IndexerState i d c                           -- ^ the initial empty indexer state
                                -> IO (IndexerState i d c)                      -- ^ result is a state consisting of the index and the map of indexed documents

stdIndexer resumeLoc startUris config eis
                                = do
                                  (_, ixState) <- runCrawler action config (initCrawlerState eis)
                                  return (getS theResultAccu ixState)
    where
    action                      = maybe (crawlDocs startUris) crawlerResume $ resumeLoc

-- ------------------------------------------------------------
 end of HolIndex i variant
-}
-- ------------------------------------------------------------

