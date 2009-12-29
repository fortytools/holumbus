-- ----------------------------------------------------------------------------

{- |
  Module     : Hayoo.Crawl
  Copyright  : Copyright (C) 2009 Sebastian M. Schlatt
  License    : MIT

  Maintainer : Sebastian M. Schlatt (sms@holumbus.org)
  Stability  : experimental
  Portability: untested
  Version    : 0.3.0

	Crawler module for the Haskell API Documentation Search Engine Hayoo!

-}

-- ----------------------------------------------------------------------------

{-# LANGUAGE Arrows #-}

module Hayoo.Crawl 


where

import           Hayoo.Common
import           Hayoo.Config

import           Data.Binary
--import           Holumbus.Common.MRBinary

import           System.Log.Logger

-- import qualified Holumbus.FileSystem.FileSystem as FS

import           Holumbus.MapReduce.Types

import Control.Parallel.Strategies


import           Data.Maybe

import qualified Data.Set    as S

import           Holumbus.Build.Crawl
import           Holumbus.Build.Config

import           Holumbus.Index.Common

import           Text.XML.HXT.Arrow     


instance NFData (CrawlerState d a)
  where
    rnf cs = ()
{-     cs 
      { cs_toBeProcessed  = rnf (cs_toBeProcessed   cs)
      , cs_wereProcessed  = rnf (cs_wereProcessed   cs)
      , cs_docHashes      = rnf (cs_docHashes       cs)
      , cs_nextDocId      = rnf (cs_nextDocId       cs)
      , cs_readAttributes = rnf (cs_readAttributes  cs)
      , cs_tempPath       = rnf (cs_tempPath        cs)
      , cs_crawlerTimeOut = rnf (cs_crawlerTimeOut  cs)
      , cs_docs           = rnf (cs_docs            cs)
      -}
    



-- ----------------------------------------------------------------------------
-- Crawling functions
-- ----------------------------------------------------------------------------

-- | dummy function for the custom information in the Documents. This is used in the crawl phase
--   of the index generation. The real custom information is computed during the split phase where
--   every document is split up into pseude-documents which contain exactly one function
customCrawlFunc :: IOSArrow XmlTree (Maybe FunctionInfo)
customCrawlFunc = constA Nothing


localLogger :: String
localLogger = "Hayoo!"

type MD5Hash = String

mapHayooCrawl :: ActionEnvironment -> CrawlerState d a -> DocId -> URI -> IO [( (),(MD5Hash, Maybe (Document FunctionInfo), S.Set URI)  )]
mapHayooCrawl _ _ k v
  = do
    infoM localLogger "mapHayooCrawl"
    debugM localLogger $ ("input: " ++ show k ++ " - " ++ show v)

    v' <- crawlDoc 0 standardReadDocumentAttributes Nothing getHtmlReferences customCrawlFunc k v

    debugM localLogger $ show $ "output: " ++ show v'
    return v'


reduceHayooCrawl :: (HolDocuments d a, Binary a, Show a)
                    => d a
                    -> ActionEnvironment                           -- ^ type whitness
                    -> CrawlerState d a 
                    -> ()                                          -- ^ no keys
                    -> [(MD5Hash, Maybe (Document a), S.Set URI)]  -- ^ data produced in the crawl phase
                    -> IO (Maybe (CrawlerState d a))
reduceHayooCrawl _ _ oldCs k vs 
  = do
    infoM localLogger "reduceHayooCrawl"
--    debugM localLogger $ show ("input: " ++ show k ++ " - " ++ show vs)
    s <- processCrawlResults' hackageFilter oldCs k vs
--    runX (traceMsg 0 (show vs))
--    debugM localLogger $ show $ "output: " ++ show s
    return s


