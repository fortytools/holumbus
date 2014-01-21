{-# LANGUAGE OverloadedStrings #-}

-- ------------------------------------------------------------

module Hayoo.Hayoo2.FctIndexerCore
where

import           Control.DeepSeq

import           Data.Binary                  (Binary)
import qualified Data.Binary                  as B
import qualified Data.ByteString.Lazy         as LB
import           Data.Maybe

import           Hayoo.FunctionInfo
import           Hayoo.Hayoo2.PostToServer
import           Hayoo.Hayoo2.RawCrawlerDoc
import           Hayoo.IndexTypes

import           Holumbus.Crawler
import           Holumbus.Crawler.IndexerCore

import           System.Directory
import           System.FilePath

import           Text.XML.HXT.Core

-- ------------------------------------------------------------

type FctCrawlerConfig = IndexCrawlerConfig () DummyDocs FunctionInfo
type FctCrawlerState  = IndexCrawlerState  () DummyDocs FunctionInfo

type FctIndexerState  = IndexerState       () DummyDocs FunctionInfo

newtype DummyDocs a = DummyDocs ()

instance NFData (DummyDocs a)

instance Binary (DummyDocs a) where
    put _ = return ()
    get   = return emptyDummyDocs

emptyDummyDocs :: DummyDocs a
emptyDummyDocs = DummyDocs ()

emptyFctState :: FctIndexerState
emptyFctState = emptyIndexerState () emptyDummyDocs

-- ------------------------------------------------------------

unionHayooFctStatesM        :: FctIndexerState -> FctIndexerState -> IO FctIndexerState
unionHayooFctStatesM _ixs1 _ixs2
    = return emptyFctState

insertHayooFctM :: ((URI, RawDoc FunctionInfo) -> IO ()) ->
                   (URI, RawDoc FunctionInfo) ->
                   FctIndexerState ->
                   IO FctIndexerState
insertHayooFctM flush rd@(_rawUri, (rawContexts, _rawTitle, _rawCustom)) ixs
    | nullContexts              = return ixs    -- no words found in document,
                                                -- so there are no refs in index
                                                -- and document is thrown away
    | otherwise                 = do flush rd
                                     return ixs
    where
    nullContexts                = and . map (null . snd) $ rawContexts

flushToFile :: (URI, RawDoc FunctionInfo) -> IO ()
flushToFile rd@(_rawUri, (_rawContexts, rawTitle, rawCustom))
    = do createDirectoryIfMissing True dirPath
         flushRawCrawlerDoc True (LB.writeFile filePath) [(RCD rd)]
      where
        dirPath  = "functions" </> pn
        filePath = dirPath </> mn ++ "." ++ fn ++ ".js"
        cs = fromJust rawCustom
        pn = package cs
        mn = moduleName cs
        fn = map esc rawTitle
             where
               esc c
                   | c `elem` "/\\" = '.'
                   | otherwise      = c

flushToServer :: String -> (URI, RawDoc FunctionInfo) -> IO ()
flushToServer url rd
    = flushRawCrawlerDoc False flush [(RCD rd)]
    where
      flush bs
          = postToServer $ mkPostReq url "insert" bs

-- ------------------------------------------------------------

-- the pkgIndex crawler configuration

indexCrawlerConfig :: ((URI, RawDoc FunctionInfo) -> IO ())
                      -> SysConfig                                    -- ^ document read options
                      -> (URI -> Bool)                                -- ^ the filter for deciding, whether the URI shall be processed
                      -> Maybe (IOSArrow XmlTree String)              -- ^ the document href collection filter, default is 'Holumbus.Crawler.Html.getHtmlReferences'
                      -> Maybe (IOSArrow XmlTree XmlTree)             -- ^ the pre document filter, default is the this arrow
                      -> Maybe (IOSArrow XmlTree String)              -- ^ the filter for computing the document title, default is empty string
                      -> Maybe (IOSArrow XmlTree FunctionInfo)        -- ^ the filter for the cutomized doc info, default Nothing
                      -> [IndexContextConfig]                         -- ^ the configuration of the various index parts
                      -> FctCrawlerConfig                             -- ^ result is a crawler config

indexCrawlerConfig flush
    = indexCrawlerConfig' (insertHayooFctM flush) unionHayooFctStatesM

-- ------------------------------------------------------------
