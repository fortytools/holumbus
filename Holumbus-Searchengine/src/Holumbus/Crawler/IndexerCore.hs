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
    , unionIndexerStatesM
    , insertRawDocM
    )
where

-- ------------------------------------------------------------

import           Control.DeepSeq
import           Control.Monad                  ( foldM )
import           Control.Monad.Trans            -- ( MonadIO )

import           Data.Binary                    ( Binary )
import qualified Data.Binary                    as B
import           Data.Function.Selector
import           Data.Maybe

import           Holumbus.Crawler

import           Holumbus.Index.Common          hiding ( URI )

import           Text.XML.HXT.Core

-- ------------------------------------------------------------

type RawDoc c                   = (RawContexts, RawTitle, Maybe c)      -- c is the user defined custom info
type RawContexts                = [RawContext]
type RawContext                 = (Context, RawWords)
type RawWords                   = [RawWord]
type RawWord                    = (Word, Position)
type RawTitle                   = String

type IndexCrawlerConfig i d c   = CrawlerConfig (RawDoc c) (IndexerState i d c)
type IndexCrawlerState  i d c   = CrawlerState             (IndexerState i d c)

data IndexContextConfig         = IndexContextConfig
                                  { ixc_name           :: String
                                  , ixc_collectText    :: IOSArrow XmlTree String
                                  , ixc_textToWords    :: String -> [String]
                                  , ixc_boringWord     :: String -> Bool
                                  }   

data IndexerState i d c         = IndexerState
                                  { ixs_index           :: ! i          -- the index type
                                  , ixs_documents       :: ! (d c)      -- the type for document descriptions
                                  } deriving (Show)

-- ------------------------------------------------------------

instance (NFData i, NFData (d c)) => NFData (IndexerState i d c)
    where
    rnf IndexerState { ixs_index     = i
                     , ixs_documents = d
                     }          = rnf i `seq` rnf d

-- ------------------------------------------------------------

instance (Binary i, Binary (d c)) => Binary (IndexerState i d c)
    where
    put s                       = B.put (ixs_index s)
                                  >>
                                  B.put (ixs_documents s)
    get                         = do
                                  ix <- B.get
                                  dm <- B.get
                                  return $ IndexerState
                                             { ixs_index          = ix
                                             , ixs_documents      = dm
                                             }

-- ------------------------------------------------------------

instance (XmlPickler i, XmlPickler (d c)) => XmlPickler (IndexerState i d c)
    where
    xpickle                     = xpElem "index-state" $
                                  xpWrap ( uncurry IndexerState
                                         , \ ix -> (ixs_index ix, ixs_documents ix)
                                         ) $
                                  xpPair xpickle xpickle

-- ------------------------------------------------------------

emptyIndexerState               :: i -> d c -> IndexerState i d c
emptyIndexerState eix edm       = IndexerState
                                  { ixs_index           = eix
                                  , ixs_documents       = edm
                                  }

-- ------------------------------------------------------------

stdIndexer                      :: ( Binary i
                                   , Binary (d c)
                                   , Binary c
                                   , HolIndexM IO i
                                   , HolDocuments d c
                                   , NFData i
                                   , NFData (d c)
                                   , NFData c) =>
                                   IndexCrawlerConfig i d c     -- ^ adapt configuration to special needs,
                                                                --   use id if default is ok
                                -> Maybe String                 -- ^ resume from interrupted index run with state
                                                                --   stored in file
                                -> [URI]                        -- ^ start indexing with this set of uris
                                -> IndexerState i d c           -- ^ the initial empty indexer state
                                -> IO (IndexCrawlerState i d c) -- ^ result is a state consisting of the index and the map of indexed documents

stdIndexer config resumeLoc startUris eis
                                = execCrawler action config (initCrawlerState eis)
    where
    action                      = do
                                  noticeC "indexerCore" ["indexer started"]
                                  res <- maybe (crawlDocs startUris) crawlerResume $ resumeLoc
                                  noticeC "indexerCore" ["indexer finished"]
                                  return res

-- ------------------------------------------------------------

-- general HolIndexM IO i version, for old specialized version see code at end of this file

indexCrawlerConfig              :: ( HolIndexM IO i
                                   , HolDocuments d c
                                   , HolDocIndex  d c i
                                   , NFData i
                                   , NFData c
                                   , NFData (d c)
                                   ) =>
                                   SysConfig                                    -- ^ document read options
                                -> (URI -> Bool)                                -- ^ the filter for deciding, whether the URI shall be processed
                                -> Maybe (IOSArrow XmlTree String)              -- ^ the document href collection filter, default is 'Holumbus.Crawler.Html.getHtmlReferences'
                                -> Maybe (IOSArrow XmlTree XmlTree)             -- ^ the pre document filter, default is the this arrow
                                -> Maybe (IOSArrow XmlTree String)              -- ^ the filter for computing the document title, default is empty string
                                -> Maybe (IOSArrow XmlTree c)                   -- ^ the filter for the cutomized doc info, default Nothing
                                -> [IndexContextConfig]                         -- ^ the configuration of the various index parts
                                -> IndexCrawlerConfig i d c                     -- ^ result is a crawler config

indexCrawlerConfig opts followRef getHrefF preDocF titleF0 customF0 contextCs
                                = addSysConfig (defaultOpts >>> opts)           -- install the default read options
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

    defaultOpts                 = withRedirect yes
                                  >>>
                                  withAcceptedMimeTypes ["text/html", "text/xhtml"]
                                  >>>
                                  withInputEncoding isoLatin1
                                  >>>
                                  withEncodingErrors no                                  -- encoding errors and parser warnings are boring
                                  >>>
                                  withValidate no
                                  >>>
                                  withParseHTML yes
                                  >>>
                                  withWarnings no

-- ------------------------------------------------------------

unionIndexerStatesM             :: ( MonadIO m
                                   , HolIndexM m i
                                   , HolDocuments d c
                                   , HolDocIndex d c i
                                   ) =>
                                   IndexerState i d c
                                -> IndexerState i d c
                                -> m (IndexerState i d c)
unionIndexerStatesM ixs1 ixs2
    = return
      $! IndexerState { ixs_index        = ix
                      , ixs_documents    = dt
                      }
    where
      (! dt, ! ix)              = unionDocIndex dt1 ix1 dt2 ix2
      ix1                       = ixs_index     ixs1
      ix2                       = ixs_index     ixs2
      dt1                       = ixs_documents ixs1
      dt2                       = ixs_documents ixs2

-- ------------------------------------------------------------

insertRawDocM                   :: ( MonadIO m
                                   , HolIndexM m i
                                   , HolDocuments d c
                                   , NFData i
                                   , NFData c
                                   , NFData (d c)
                                   ) =>
                                   (URI, RawDoc c)                              -- ^ extracted URI and doc info
                                -> IndexerState i d c                           -- ^ old indexer state
                                -> m (IndexerState i d c)                       -- ^ new indexer state

insertRawDocM (rawUri, (rawContexts, rawTitle, rawCustom)) ixs
    | nullContexts              = return ixs    -- no words found in document,
                                                -- so there are no refs in index
                                                -- and document is thrown away
    | otherwise                 = do
                                  newIx  <- foldM (insertRawContextM did) (ixs_index ixs) $ rawContexts
                                  newIxs <- return $
                                            IndexerState { ixs_index        = newIx
                                                         , ixs_documents    = newDocs
                                                         }
                                  rnf newIxs `seq`
                                      return newIxs
    where
    nullContexts                = and . map (null . snd) $ rawContexts
    (did, newDocs)              = insertDoc (ixs_documents ixs) doc
    doc                         = Document
                                  { title       = rawTitle
                                  , uri         = rawUri
                                  , custom      = rawCustom
                                  }

insertRawContextM               :: (Monad m, HolIndexM m i) =>
                                   DocId -> i -> (Context, [(Word, Position)]) -> m i
insertRawContextM did ix (cx, ws)
                                = foldM (insWordM cx did) ix ws

insWordM                        :: (Monad m, HolIndexM m i) =>
                                   Context -> DocId -> i -> (Word, Position) -> m i
insWordM cx' did' ix' (w', p')  = insertPositionM cx' w' did' p' ix'

-- ------------------------------------------------------------
