{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
where

import		 TextDocs

import           Data.Function.Selector

import           Data.List
import		 Data.Maybe			( )
import qualified Data.Map       		as M

import		 Holumbus.Crawler.Core
import           Holumbus.Crawler.Html
import           Holumbus.Crawler.Util

import           System.IO
import		 System.Environment

import		 Text.XML.HXT.Arrow		hiding ( when
						       , getState
						       )
import qualified Text.XML.HXT.Arrow		as X

-- ------------------------------------------------------------

getOptions		:: [String] -> (Maybe String, String)
getOptions ("-r":fn:as)	= (Just fn, r)
			  where
			  (_, r) = getOptions as
getOptions (out:_)	= (Nothing, out)
getOptions []		= (Nothing, "")

main	:: IO ()
main	= do
	  (resume, out) <- getArgs >>= return . getOptions
	  let action	= maybe (crawlDocs ["file:///home/uwe/haskell/crawl2/examples/fussballsprueche/docs/index.php"])
				crawlerResume
				$
				resume
	  (_, docs) <- runCrawler action
                                  testCrawlerConfig
				  textCrawlerInitState
	  runX ( constA (getS theResultAccu $ docs)
		 >>>
		 xpickleVal (xpTextDocs "http://localhost/~si/")
		 >>>
		 addXHtmlDoctypeTransitional
		 >>>
		 writeDocument [ (a_indent, v_1)
			       , (a_output_encoding, usAscii)
			       , (a_output_html, v_1)
			       ] out
	       )
	  return ()

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
			  xshow ( ( theJokes )
				  >>>
				  deep isText
				  -- >>>
				  -- arr ( D.trace "extractText" )		-- test: make evaluation order visible
				)
			  >>^ (words >>> unwords)

    theJokes		= ( multi
			    $
			    hasName "tr"
		            >>>
			    has (this /> hasName "td" /> (hasName "div" >>> hasAttrValue "class" (== "showName")) /> (hasName "a" >>> hasAttrValue "class" (== "showName")))
			  ) -- >>. (drop 1 . take 1)

    has t               = deep t `guards` this

textCrawlerInitState	:: CrawlerState TextDocs
textCrawlerInitState	= initCrawlerState emptyTextDocs

-- ------------------------------------------------------------

testCrawlerConfig 	:: CrawlerConfig TextDoc TextDocs
testCrawlerConfig	= setS theFollowRef
			  ( simpleFollowRef'
			    [ "file:///home/uwe/haskell/crawl2/examples/fussballsprueche/docs/index.php"
			    ]
			    [ ".*"
			    ]
			  )
			  >>>
			  setS theMaxNoOfDocs 2000						-- limit of docs to be crawled
			  >>>
			  setS theSaveIntervall 20						-- every 20 documents the state is saved
			  >>>
			  setS theSavePathPrefix "./tmp/fussballsprueche-"					-- states are saved in subdir "./tmp" in files starting with "hc-"
			  >>>
			  setS theTraceLevel 1							-- trace actions with lowest level
			  $
			  textCrawlerConfig

-- ------------------------------------------------------------
