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

import           Holumbus.Index.Common          ( loadFromBinFile )

import           System.IO
import		 System.Environment
import		 Text.XML.HXT.Arrow

-- ------------------------------------------------------------

testCrawlerConfig 	:: CrawlerConfig TextDoc TextDocs
testCrawlerConfig	= store theFollowRef ( simpleFollowRef'
			                       [ "http://localhost/~si/klausuren/.*[.]html"			-- we'll follow these URIs
			                       , "http://localhost/~si/termine/.*[.]html"
					       , "http://localhost/~si/vorlesungen/fp/.*[.]html"
					       , "http://localhost/~si/vorlesungen/cb/.*[.]html"
					       , "http://localhost/~si/vorlesungen/java/.*[.]html"
					       , "http://localhost/~si/vorlesungen/softwaredesign/.*[.]html"
					       , "http://localhost/~si/vorlesungen/internet/.*[.]html"
					       ]
			                       [ ".*/welcome.html"						-- except welcome.html and URIs with query string
					       , ".*[?].*"
					       , "http://localhost/~si/vorlesungen/internet/handouts/.*"
					       ]
					     )
			  >>>
			  store theMaxNoOfDocs 2000
			  >>>
			  store theSaveIntervall 10
			  >>>
			  store theTraceLevel 1
			  $
			  textCrawlerConfig

main	:: IO ()
main	= do
	  [out]     <- getArgs
	  (_, docs) <- runCrawler (crawlDocs ["http://localhost/~si/"])
                                  testCrawlerConfig
				  textCrawlerInitState
	  runX ( constA (load theResultAccu $ docs)
		 >>>
		 xpickleVal (xpTextDocs "http://localhost/~si/")
		 >>>
		 addXHtmlDoctypeTransitional
		 >>>
		 writeDocument [ (a_indent, v_1)
			       , (a_output_encoding, usAscii)
			       ] out
	       )
	  return ()

-- ------------------------------------------------------------

-- just for checking temorarily saved intermedediate crawler states

sss	:: String -> IO ()
sss fn	= do
	  s <- loadFromBinFile fn
	  putStrLn $ show (s::CrawlerState TextDocs)

-- ------------------------------------------------------------
