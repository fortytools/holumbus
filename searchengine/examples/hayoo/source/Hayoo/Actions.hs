-- ----------------------------------------------------------------------------
{- |
  Module     : Hayoo.Actions
  Copyright  : Copyright (C) 2009 Sebastian M. Schlatt
  License    : MIT

  Maintainer : Sebastian Schlatt (sms@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Hayoo.Actions where


import           Holumbus.Index.Documents
import           Holumbus.Index.Common
import           Holumbus.Build.Config

import           Data.Set (Set)
import qualified Data.Set as S

import           Holumbus.MapReduce.Types

import           Hayoo.Crawl
import           Hayoo.Common
    

hayooCrawlAction :: 
  ActionConfiguration 
    (CrawlerState Documents FunctionInfo)
    DocId URI
    () (MD5Hash, Maybe (Document FunctionInfo), Set URI)
    (MD5Hash, Maybe (Document FunctionInfo), Set URI) (CrawlerState Documents FunctionInfo)
hayooCrawlAction  
  = (defaultActionConfiguration "HAYOO_CRAWL")
  { ac_Map     = Just mapAction
--  , ac_Combine = Nothing
  , ac_Reduce  = Just reduceAction
  }
  where
    mapAction     = (defaultMapConfiguration mapHayooCrawl)
    reduceAction  = (defaultReduceConfiguration (reduceHayooCrawl (emptyDocuments :: Documents FunctionInfo)) )
  
