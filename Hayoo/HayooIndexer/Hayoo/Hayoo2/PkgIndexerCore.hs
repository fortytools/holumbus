{-# LANGUAGE OverloadedStrings #-}

-- ------------------------------------------------------------

module Hayoo.Hayoo2.PkgIndexerCore
where

import           Control.Applicative          ((<$>))
import           Control.DeepSeq

import           Data.Aeson
import           Data.Binary                  (Binary)
import qualified Data.Binary                  as B
import qualified Data.ByteString.Lazy         as LB
import           Data.Maybe
import qualified Data.StringMap.Strict        as M

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

type PkgCrawlerConfig = IndexCrawlerConfig () RawDocIndex PackageInfo
type PkgCrawlerState  = IndexCrawlerState  () RawDocIndex PackageInfo

type PkgIndexerState  = IndexerState       () RawDocIndex PackageInfo

newtype RawDocIndex a   = RDX (M.StringMap (RawDoc PackageInfo))
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

flushToFile :: String -> PkgIndexerState -> IO ()
flushToFile pkgName fx
    = do createDirectoryIfMissing True dirPath
         flushTo (flushRawCrawlerDoc True (LB.writeFile filePath)) fx
      where
        dirPath  = "json"
        filePath = dirPath </> pkgName ++ ".js"

flushToServer :: String -> PkgIndexerState -> IO ()
flushToServer url fx
    = flushTo (flushRawCrawlerDoc False flush) fx
    where
      flush bs
          = postToServer $ mkPostReq url "insert" bs

flushTo :: ([RawCrawlerDoc PackageInfo] -> IO ()) -> PkgIndexerState -> IO ()
flushTo flush (IndexerState _ (RDX ix))
    | M.null ix
        = return ()
    | otherwise
        = flush $ map RCD (M.toList ix)

{- old stuff
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
-- -}
-- ------------------------------------------------------------

toRankDocs :: Documents PackageInfo -> [ToRank]
toRankDocs = filter (\ (ToRank _ d) -> d /= defPackageRank) . map toRank . elemsDocIdMap . toMap

toRank :: Document PackageInfo -> ToRank
toRank d = ToRank (uri d) (p_rank . fromJust . custom $ d)


toRankRawDocIndex :: PkgIndexerState -> [ToRank]
toRankRawDocIndex (IndexerState _ (RDX dt))
    = map toR . M.toList $ dt
      where
        toR (url, (_c, _t, Just cs)) = ToRank url (p_rank cs)
        toR _                        = error "toRankRawDocIndex: No custom info"

data ToRank = ToRank URI Score

instance ToJSON ToRank where
    toJSON (ToRank u r)
        = object $
          [ "uri"         .= u
          , "description" .= (object ["pkg-rank" .= show r]) -- convert rank to string
          ]

flushRanksToFile :: String -> Documents PackageInfo -> IO ()
flushRanksToFile path0 dt
    = flushRawCrawlerDoc True (LB.writeFile path) (toRankDocs dt)
      where
        path = "json/" ++ path0 ++ ".js"

flushRanksToServer :: String -> Documents PackageInfo -> IO ()
flushRanksToServer url dt
    = flushRawCrawlerDoc False flush (toRankDocs dt)
    where
      flush bs
          = postToServer $ mkPostReq url "update" bs

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
