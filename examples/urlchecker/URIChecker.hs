{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
where

import           Data.Function.Selector

import		 Data.Maybe			( )

import		 Holumbus.Crawler.Core
import           Holumbus.Crawler.URIChecker
-- import           Holumbus.Crawler.Util

import           System.IO
import		 System.Environment
import		 Text.XML.HXT.Arrow

-- ------------------------------------------------------------

checkURIAction 		:: [(String, URIClass)]
checkURIAction		= [ ("http://localhost/~si/", 			Contents)
			  , ("http://localhost/~si/.*/welcome[.]html", 	Manual)
			  , ("http://localhost/~si/index*[.]html", 	Contents)
			  , ("http://localhost/~si.*[.]html",		Exists)
			  , ("http://localhost/~si/.*[.](gif|jpg|css|ico|pdf)",		Exists)
			  , ("http://localhost/.*", 			Manual)
			  , ("http://.*", 				Manual)
			  , ("file:///.*", 				Illegal)
			  , ("mailto:.*", 				Manual)
			  , ("https://.*", 				Manual)
			  , ("javascript:.*", 				Ignore)
			  ]

checkURIConfig 		:: URICrawlerConfig
checkURIConfig		= setS theMaxNoOfDocs 2000						-- limit of docs to be crawled
			  >>>
			  setS theSaveIntervall 20						-- every 20 documents the state is saved
			  >>>
			  setS theSavePathPrefix "./tmp/check-"					-- states are saved in subdir "./tmp" in files starting with "hc-"
			  >>>
			  setS theTraceLevel 1							-- trace actions with lowest level
			  $
			  uriCrawlerConfig (simpleURIClassifier checkURIAction)

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
                                  checkURIConfig
				  uriCrawlerInitState
	  putStrLn $ show docs
{-
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
-}
	  return ()

-- ------------------------------------------------------------
