{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
where

import		 TextDocs

import           Data.Function.Selector

import		 Data.Maybe			( )

import		 Holumbus.Crawler.Core
import           Holumbus.Crawler.HtmlText
import           Holumbus.Crawler.Util

import           System.IO
import		 System.Environment
import		 Text.XML.HXT.Arrow

-- ------------------------------------------------------------

testCrawlerConfig 	:: CrawlerConfig TextDoc TextDocs
testCrawlerConfig	= setS theFollowRef
			  ( simpleFollowRef'
			    [ "http://localhost/~si/klausuren/.*[.]html"			-- we'll follow these URIs
			    , "http://localhost/~si/termine/.*[.]html"
			    , "http://localhost/~si/vorlesungen/fp/.*[.]html"
			    , "http://localhost/~si/vorlesungen/cb/.*[.]html"
			    , "http://localhost/~si/vorlesungen/java/.*[.]html"
			    , "http://localhost/~si/vorlesungen/softwaredesign/.*[.]html"
			    , "http://localhost/~si/vorlesungen/internet/.*[.]html"
			    ]
			    [ ".*/welcome.html"							-- except welcome.html and URIs with query string
			    , ".*[?].*"
			    , "http://localhost/~si/vorlesungen/internet/handouts/.*"
			    ]
			  )
			  >>>
			  setS theMaxNoOfDocs 2000						-- limit of docs to be crawled
			  >>>
			  setS theSaveIntervall 20						-- every 20 documents the state is saved
			  >>>
			  setS theSavePathPrefix "./tmp/hc-"					-- states are saved in subdir "./tmp" in files starting with "hc-"
			  >>>
			  setS theTraceLevel 1							-- trace actions with lowest level
			  $
			  textCrawlerConfig

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
	  let action	= maybe (crawlDocs ["http://localhost/~si/"])
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
