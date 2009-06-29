{-# OPTIONS #-}

-- ------------------------------------------------------------

module SimpleIndexer
where

import		 Holumbus.Crawler.IndexerCore

import		 Holumbus.Index.Inverted.Memory

type PlainText			= String

-- ------------------------------------------------------------

type SimpleIndexerState		= IndexerState Inverted PlainText

simpleIndexerState		:: SimpleIndexerState
simpleIndexerState		= defaultIndexerState emptyInverted

-- ------------------------------------------------------------
