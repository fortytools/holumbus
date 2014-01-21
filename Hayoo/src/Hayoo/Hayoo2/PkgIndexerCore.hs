{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- ------------------------------------------------------------

module Hayoo.Hayoo2.PkgIndexerCore
where

-- import           Control.DeepSeq

import           Data.Aeson
import qualified Data.ByteString.Lazy         as LB
import           Data.Maybe

import           Hayoo.Hayoo2.PostToServer
import           Hayoo.Hayoo2.RawCrawlerDoc
import           Hayoo.IndexTypes
import           Hayoo.PackageInfo

import           Holumbus.Crawler
import           Holumbus.Crawler.IndexerCore
import           Holumbus.Index.Common        hiding (URI)

import           System.Directory
import           System.FilePath

import           Text.XML.HXT.Core

-- ------------------------------------------------------------

type PkgCrawlerConfig = IndexCrawlerConfig () Documents PackageInfo
type PkgCrawlerState  = IndexCrawlerState  () Documents PackageInfo

type PkgIndexerState  = IndexerState       () Documents PackageInfo

emptyPkgState :: PkgIndexerState
emptyPkgState = emptyIndexerState () emptyDocuments

-- ------------------------------------------------------------

unionHayooPkgStatesM        :: PkgIndexerState -> PkgIndexerState -> IO PkgIndexerState
unionHayooPkgStatesM ixs1 ixs2
    = return
      $! IndexerState { ixs_index        = ()
                      , ixs_documents    = dt
                      }
    where
      !dt = unionDocIndex' (ixs_documents ixs1) (ixs_documents ixs2)

      unionDocIndex' dt1 dt2
        | s1 == 0               = dt2
        | s2 == 0               = dt1
        | s1 < s2               = unionDocIndex' dt2 dt1
        | otherwise             = dt'
        where
          dt'                   = unionDocs     dt1  dt2s
          dt2s                  = editDocIds    add1 dt2

          add1                  = addDocId disp
          max1                  = maxKeyDocIdMap . toMap $ dt1
          min2                  = minKeyDocIdMap . toMap $ dt2
          disp                  = incrDocId $ subDocId max1 min2

          s1                    = sizeDocs dt1
          s2                    = sizeDocs dt2


insertHayooPkgM :: ((URI, RawDoc PackageInfo) -> IO ()) ->
                   (URI, RawDoc PackageInfo) ->
                   PkgIndexerState ->
                   IO PkgIndexerState
insertHayooPkgM flush rd@(rawUri, (rawContexts, rawTitle, rawCustom)) ixs
    | nullContexts              = return ixs    -- no words found in document,
                                                -- so there are no refs in index
                                                -- and document is thrown away
    | otherwise                 = do flush rd
                                     return $! newIxs
    where
    nullContexts                = and . map (null . snd) $ rawContexts
    ! newIxs                    = ixs {ixs_documents = newDocs}
    ! (_did, ! newDocs)         = insertDoc (ixs_documents ixs) doc
    ! doc                       = Document
                                  { title       = rawTitle
                                  , uri         = rawUri
                                  , custom      = rawCustom
                                  }

flushToFile :: (URI, RawDoc PackageInfo) -> IO ()
flushToFile rd@(_rawUri, (_rawContexts, rawTitle, _rawCustom))
    = do createDirectoryIfMissing True dirPath
         flushRawCrawlerDoc True (LB.writeFile filePath) (RCD rd)
      where
        dirPath  = "packages"
        filePath = dirPath </> pn ++ ".js"
        pn = rawTitle

flushToServer :: String -> (URI, RawDoc PackageInfo) -> IO ()
flushToServer url rd
    = flushRawCrawlerDoc False flush [(RCD rd)]
    where
      flush bs
          = postToServer $ mkPostReq url "insert" bs

flushToDevNull :: (URI, RawDoc PackageInfo) -> IO ()
flushToDevNull = const (return ())

-- ------------------------------------------------------------

toRankDocs :: Documents PackageInfo -> [ToRank]
toRankDocs = filter (\ (ToRank _ d) -> d /= defPackageRank) . map toRank . elemsDocIdMap . toMap

toRank :: Document PackageInfo -> ToRank
toRank d = ToRank (uri d) (p_rank . fromJust . custom $ d)

data ToRank = ToRank URI Score

instance ToJSON ToRank where
    toJSON (ToRank u r)
        = object $
          [ "uri"         .= u
          , "description" .= (object ["pkg-rank" .= show r]) -- convert rank to string
          ]

flushRanksToFile :: Documents PackageInfo -> IO ()
flushRanksToFile dt
    = flushRawCrawlerDoc True (LB.writeFile path) (toRankDocs dt)
    where
      path = "packages/0000-ranks.js"

flushRanksToServer :: String -> Documents PackageInfo -> IO ()
flushRanksToServer url dt
    = flushRawCrawlerDoc False flush (toRankDocs dt)
    where
      flush bs
          = postToServer $ mkPostReq url "update" bs

-- ------------------------------------------------------------

-- the pkgIndex crawler configuration

indexCrawlerConfig           :: ((URI, RawDoc PackageInfo) -> IO ())
                                -> SysConfig                                    -- ^ document read options
                                -> (URI -> Bool)                                -- ^ the filter for deciding, whether the URI shall be processed
                                -> Maybe (IOSArrow XmlTree String)              -- ^ the document href collection filter, default is 'Holumbus.Crawler.Html.getHtmlReferences'
                                -> Maybe (IOSArrow XmlTree XmlTree)             -- ^ the pre document filter, default is the this arrow
                                -> Maybe (IOSArrow XmlTree String)              -- ^ the filter for computing the document title, default is empty string
                                -> Maybe (IOSArrow XmlTree PackageInfo)         -- ^ the filter for the cutomized doc info, default Nothing
                                -> [IndexContextConfig]                         -- ^ the configuration of the various index parts
                                -> PkgCrawlerConfig                             -- ^ result is a crawler config

indexCrawlerConfig flush
    = indexCrawlerConfig' (insertHayooPkgM flush) unionHayooPkgStatesM

-- ------------------------------------------------------------
