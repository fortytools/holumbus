{-# LANGUAGE OverloadedStrings #-}

-- ------------------------------------------------------------

module Hayoo.Hunt.PkgIndexerCore
where

import           Control.Applicative          ((<$>))
import           Control.DeepSeq

import           Data.Binary                  (Binary)
import qualified Data.Binary                  as B
import qualified Data.StringMap.Strict        as M
import qualified Data.Text                    as T
import           Data.Time

import           Hayoo.Hunt.ApiDocument
import           Hayoo.Hunt.IndexSchema
import           Hayoo.IndexTypes

import           Holumbus.Crawler
import           Holumbus.Crawler.IndexerCore
import           Holumbus.Index.Common        hiding (URI)

import           Hunt.Interpreter.Command

import           Text.XML.HXT.Core

-- ------------------------------------------------------------

type PkgCrawlerConfig = IndexCrawlerConfig () RawDocIndex PackageInfo
type PkgCrawlerState  = IndexCrawlerState  () RawDocIndex PackageInfo

type PkgIndexerState  = IndexerState       () RawDocIndex PackageInfo

newtype RawDocIndex a = RDX (M.StringMap (RawDoc PackageInfo))
                          deriving (Show)

instance NFData (RawDocIndex a)

instance Binary (RawDocIndex a) where
    put (RDX ix)        = B.put ix
    get                 = RDX <$> B.get

emptyPkgState           :: PkgIndexerState
emptyPkgState           = emptyIndexerState () emptyRawDocIndex

emptyRawDocIndex        :: RawDocIndex a
emptyRawDocIndex        = RDX $ M.empty

insertRawDoc            :: URI -> RawDoc PackageInfo -> RawDocIndex a -> RawDocIndex a
insertRawDoc url rd (RDX ix)
                        = rnf rd `seq` (RDX $ M.insert url rd ix)

-- ------------------------------------------------------------

unionHayooPkgStatesM        :: PkgIndexerState -> PkgIndexerState -> IO PkgIndexerState
unionHayooPkgStatesM (IndexerState _ (RDX dt1)) (IndexerState _ (RDX dt2))
    = return
      $! IndexerState { ixs_index        = ()
                      , ixs_documents    = RDX $ M.union dt1 dt2
                      }


insertHayooPkgM :: (URI, RawDoc PackageInfo) ->
                   PkgIndexerState ->
                   IO PkgIndexerState
insertHayooPkgM (rawUri, rawDoc@(rawContexts, _rawTitle, _rawCustom))
                ixs@(IndexerState _ (RDX dt))
    | nullContexts              = return ixs    -- no words found in document,
                                                -- so there are no refs in index
                                                -- and document is thrown away
    | otherwise
        = return $!
          IndexerState { ixs_index = ()
                       , ixs_documents = RDX $ M.insert rawUri rawDoc dt
                       }
    where
    nullContexts
        = and . map (null . snd) $ rawContexts

toCommand :: UTCTime -> PkgIndexerState -> Command
toCommand now (IndexerState _ (RDX ix))
    = Sequence . concatMap toCmd . M.toList $ ix
    where
      now'  = fmtDateXmlSchema now
      now'' = fmtDateHTTP      now

      toCmd (k, (cx, t, cu))
          = insertCmd apiDoc2
            where
              insertCmd = (:[]) . Insert
              apiDoc    = toApiDoc $ (T.pack k, (cx, t, fmap PD cu))

              -- add "package" as type to the type context for easy search of packages
              apiDoc0   = insIndexMap c'type d'package apiDoc

              -- HACK: add upload time to c'upload context
              -- to enable search for latest packages
              apiDoc1   = insIndexMap c'upload upl $
                          apiDoc0
                  where
                    upl = maybe "" id uplDate
                        where
                          uplDate
                              = do dt1 <- p_uploaddate <$> cu
                                   pd  <- parseDateHTTP dt1
                                   return $ fmtDateXmlSchema pd

              apiDoc2   = insDescrMap d'indexed now'' $
                          insIndexMap c'indexed now'  $
                          apiDoc1


-- ------------------------------------------------------------

toRankDocs :: Documents PackageInfo -> [(URI, RawDoc RankDescr)]
toRankDocs = map toRank . elemsDocIdMap . toMap

toRank :: Document PackageInfo -> (URI, ([a], String, Maybe RankDescr))
toRank d = (uri d, ([], "", fmap (RD . p_rank) $ custom d))

rankToCommand :: Documents PackageInfo -> Command
rankToCommand
    = Sequence . concatMap toCmd . toRankDocs
    where
      toCmd (k, rd)
          | boringApiDoc d = []
          | otherwise      = [Update d]
          where
            d = toApiDoc $ (T.pack k, rd)

-- ------------------------------------------------------------

-- the pkgIndex crawler configuration

indexCrawlerConfig           :: SysConfig                                    -- ^ document read options
                                -> (URI -> Bool)                                -- ^ the filter for deciding, whether the URI shall be processed
                                -> Maybe (IOSArrow XmlTree String)              -- ^ the document href collection filter, default is 'Holumbus.Crawler.Html.getHtmlReferences'
                                -> Maybe (IOSArrow XmlTree XmlTree)             -- ^ the pre document filter, default is the this arrow
                                -> Maybe (IOSArrow XmlTree String)              -- ^ the filter for computing the document title, default is empty string
                                -> Maybe (IOSArrow XmlTree PackageInfo)         -- ^ the filter for the cutomized doc info, default Nothing
                                -> [IndexContextConfig]                         -- ^ the configuration of the various index parts
                                -> PkgCrawlerConfig                             -- ^ result is a crawler config

indexCrawlerConfig
    = indexCrawlerConfig' insertHayooPkgM unionHayooPkgStatesM

-- ------------------------------------------------------------
