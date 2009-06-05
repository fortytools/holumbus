{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
where

import		 Control.Parallel.Strategies	( )

import           Data.Function.Selector

import		 Data.Maybe			( )
import qualified Data.Map as M

import		 Holumbus.Crawler.Core
import           Holumbus.Crawler.HtmlText
import           Holumbus.Crawler.Util

import           Holumbus.Index.Common          ( loadFromBinFile )

import           System.IO

import		 Text.XML.HXT.Arrow		hiding ( when
						       , getState
						       )
import qualified Text.XML.HXT.Arrow		as X

-- import qualified Debug.Trace			as D

-- ------------------------------------------------------------

xpTextDocs		:: String -> PU TextDocs
xpTextDocs ref		= xpElem "html" $
			  xpWrap ( snd
				 , \ x -> ("Content of HTML Pages of " ++ show ref, x)
				 ) $
			  xpPair ( xpElem "head" $
				   xpElem "title" $
				   xpText
				 )
			         ( xpElem "body" $
				   xpWrap ( snd
					  , \ x -> ("Content of HTML Pages of " ++ show ref, x)
					  ) $
				   xpPair ( xpElem "h1" $
					    xpText
					  )
				          ( xpWrap ( M.fromList
						   , M.toList
						   ) $
					    xpList $
					    xpPair ( xpElem "dt" $
						     xpElem "a" $
						     xpWrap ( snd
							    , \ x -> (x, x)
							    ) $
						     xpPair (xpAttr "href" $ xpText) xpText
						   )
                                            ( xpElem "dd" $ xpText )
					  )
				 )

-- ------------------------------------------------------------

testCrawlerConfig 	:: CrawlerConfig TextDoc TextDocs
testCrawlerConfig	= store theFollowRef ( simpleFollowRef'
			                       [ "http://localhost/~si/klausuren/.*[.]html"
					       , "http://localhost/~si/vorlesungen/fp/.*[.]html"
					       , "http://localhost/~si/vorlesungen/cb/.*[.]html"
					       ]
			                       [".*/welcome.html"]
					     )
			  >>>
			  store theMaxNoOfDocs 200
			  >>>
			  store theSaveIntervall 5
			  >>>
			  store theTraceLevel 1
			  $ textCrawlerConfig

t1 	:: IO TextDocs
t1	= execCrawler (crawlDoc "http://localhost/~si/") testCrawlerConfig textCrawlerInitState

t2 	:: IO ((), CrawlerState TextDocs)
t2	= runCrawler  (crawlDocs ["http://localhost/~si/"]) testCrawlerConfig textCrawlerInitState

sss	:: String -> IO ()
sss fn	= do
	  s <- loadFromBinFile fn
	  putStrLn $ show (s::CrawlerState TextDocs)

main	:: IO ()
main	= do
	  (_, docs) <- runCrawler (crawlDocs ["http://localhost/~si/"])
                                  testCrawlerConfig
				  textCrawlerInitState
	  runX ( constA ((load theResultAccu) docs)
		 >>>
		 xpickleVal (xpTextDocs "http://localhost/~si/")
		 >>>
		 addXHtmlDoctypeTransitional
		 >>>
		 writeDocument [ (a_indent, v_1)
			       , (a_output_encoding, usAscii)
			       ] "content.html"
	       )
	  return ()

-- ------------------------------------------------------------
