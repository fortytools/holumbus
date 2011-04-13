{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.CrawlerAction
where

import 		 Control.Monad.Reader
import		 Control.Monad.State
import 		 Control.Monad.ReaderStateIO

import           Data.Function.Selector

import           Holumbus.Crawler.Types

-- import qualified Debug.Trace			as D

-- ------------------------------------------------------------

type CrawlerAction a r x	= ReaderStateIO (CrawlerConfig a r) (CrawlerState r) x

-- ------------------------------------------------------------
--
-- basic crawler actions

-- | Load a component from the crawler configuration

getConf				:: Selector (CrawlerConfig a r) v -> CrawlerAction a r v
getConf				= asks . getS

getState			:: Selector (CrawlerState r) v -> CrawlerAction a r v
getState 			= gets . getS

putState			:: Selector (CrawlerState r) v -> v -> CrawlerAction a r ()
putState sel			= modify . setS sel

modifyState			:: Selector (CrawlerState r) v -> (v -> v) -> CrawlerAction a r ()
modifyState sel			= modify . chgS sel

modifyStateIO			:: Selector (CrawlerState r) v -> (v -> IO v) -> CrawlerAction a r ()
modifyStateIO sel		= modifyIO . chgM sel

-- ------------------------------------------------------------
