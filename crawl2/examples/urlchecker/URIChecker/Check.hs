{-# OPTIONS #-}

-- ------------------------------------------------------------

module URIChecker.Check
    ( main1
    , URIClass(..)
    , URIClassList
    )
where

import           Holumbus.Crawler
import           Holumbus.Crawler.URIChecker

import           System.IO
import		 System.Environment

import		 Text.XML.HXT.Arrow
import           Text.XML.HXT.Arrow.XmlCache

import		 URIChecker.Template

-- ------------------------------------------------------------

getOptions		:: [String] -> (Maybe String, String, String)
getOptions ("-r":fn:as)	= (Just fn, s, r)
			  where
			  (_, s, r) = getOptions as
getOptions (uri : out : _)
    			= (Nothing, uri, out)
getOptions (uri : _)	= (Nothing, uri, "")
getOptions []		= (Nothing, "", "")

main1			:: [(String, URIClassList)] -> IO ()
main1 sessList		= do
			  (resume, sid, out) <- getArgs >>= return . getOptions
			  case lookup sid sessList of
			    Nothing -> hPutStrLn stderr $ unlines $
				         [ unwords ["no URI config for start URI found:", show sid]
				         , ""
					 , "possible start uris are"
					 , ""
					 ]
				         ++ map fst sessList

			    Just uris -> runX ( hxtSetTraceAndErrorLogger DEBUG
						>>>
					        arrIO0 (simpleURIChecker resume sid uris)
						>>>
						genResultPage out sid uris
					      )
					 >> return ()

					 -- dm <- stdURIChecker 8096 64 "/tmp/hc-check-" 1 [(curl_max_filesize, "1000000")] resume sid uris

-- ------------------------------------------------------------

simpleURIChecker	:: Maybe String -> URI -> URIClassList -> IO DocMap
simpleURIChecker	= stdURIChecker
                          ( 10000						-- limit total number of documents
			  , 100							-- # of documents analysed in parallel
			  , 10							-- # of thread running in parallel
                          )
                          ( 500							-- save intermediate state every 250 documents
                          , "./tmp/uri-check-"					-- path prefix for saving intermediate states
                          )
                          ( NOTICE						-- set trace level to 1
                          , NOTICE						-- set trace level for hxt to 1
                          )
                          [ (a_cache, 	"./cache"	)			-- local cache dir "cache"
			  , (a_compress, v_1		)			-- cache files will be compressed
			  , (a_document_age,
			     show $ (60 * 60 * 24 * 1::Integer))		-- cache remains valid 1 day
			  , (curl_max_filesize, 	"1000000"	)	-- limit document size to 1 Mbyte
			  , (a_ignore_encoding_errors, 	v_1		)    	-- encoding errors and parser warnings are boring
			  , (a_issue_warnings, 		v_0		)
			  , (curl_location, 		v_1		)	-- automatically follow redirects
			  , (curl_max_redirects, 	"3"		)	-- but limit # of redirects to 3
			  , (a_accept_mimetypes, 	"text/html"	)
			  ]
								      

-- ------------------------------------------------------------
