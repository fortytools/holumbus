{-# OPTIONS #-}

-- ------------------------------------------------------------
-- |
-- A test crawler:
-- Crawls a collection of documents and extracts the text from the documents.
-- Result is a map from URIs to a string of words found in the 

module Holumbus.Crawler.HtmlText
where

import           Data.Function.Selector

-- import           Data.List
-- import		 Data.Maybe			( )

import qualified Data.Map       		as M

import		 Holumbus.Crawler.URIs
import		 Holumbus.Crawler.Core
import           Holumbus.Crawler.Html

import		 Text.XML.HXT.Arrow		hiding ( when
						       , getState
						       )
-- import qualified Text.XML.HXT.Arrow		as X

-- import qualified Debug.Trace			as D

-- ------------------------------------------------------------

type TextDoc		= String

type TextDocs		= M.Map URI TextDoc

type TextCrawlerConfig	= CrawlerConfig TextDoc TextDocs

emptyTextDocs		:: TextDocs
emptyTextDocs		= M.empty

textCrawlerConfig	:: TextCrawlerConfig
textCrawlerConfig	= addReadAttributes  [ ]				-- at the moment no more read attributes are neccessary
			  >>>
			  setS theFollowRef 	followRefs
			  >>>
			  setS thePreDocFilter	documentOK
			  >>>
			  setS theProcessDoc	extractText
			  $
			  baseConfig
    where
    baseConfig 		= defaultHtmlCrawlerConfig insertTextDoc		-- take the default HTML crawler config
										-- and set the accumulator op
    insertTextDoc	:: AccumulateDocResult TextDoc TextDocs
    insertTextDoc x	= return . uncurry M.insert x

    followRefs		= const True						-- all hrefs are collected

    documentOK		= ( getAttrValue transferStatus >>> isA (== "200") )	-- document transfer status must be 200 OK
			  `guards`
			  this

    extractText		= fromLA $ rnfA $					-- force complete evaluation of the result: this is essential, don't delete rnfA
			  xshow ( ( theTitle <+> theBody )
				  >>>
				  deep isText
				  -- >>>
				  -- arr ( D.trace "extractText" )		-- test: make evaluation order visible
				)
			  >>^ (words >>> unwords)

    theBody		= this /> hasName "html" /> hasName "body"
    theTitle		= this /> hasName "html" /> hasName "head" /> hasName "title"

textCrawlerInitState	:: CrawlerState TextDocs
textCrawlerInitState	= initCrawlerState emptyTextDocs

-- ------------------------------------------------------------
