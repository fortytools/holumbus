{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
where

import		 Holumbus.Crawler
import		 Holumbus.Crawler.CacheCore

import           System.Environment

import		 Text.XML.HXT.Arrow
import           Text.XML.HXT.Arrow.XmlCache

-- ------------------------------------------------------------

simpleCacher			:: Maybe String -> [URI] -> (URI -> Bool) -> IO CacheCrawlerState
simpleCacher			= stdCacher
                                  (5000, 5, 5)						-- max docs, max par docs, max threads
                                  (500, "./tmp/ix-")					-- save intervall and path
                                  (DEBUG,NOTICE)					-- log cache and hxt
				  [ (a_cache, 	"./cache"	)			-- local cache dir "cache"
				  , (a_compress, v_1		)			-- cache files will be compressed
				  , (a_document_age,
					 show $ (60 * 60 * 24 * 1::Integer))		-- cache remains valid 1 day
                                  , (a_accept_mimetypes, 	unwords [text_html, application_xhtml, application_pdf, text_plain])
				  , (a_parse_html,              v_0)
				  , (a_parse_by_mimetype,	v_1)
				  ]                                                     -- use default read options, but accept pdfs too
				  id							-- no more config stuff

-- ------------------------------------------------------------

siCacher                       :: Maybe String -> IO CacheCrawlerState
siCacher resume                = simpleCacher resume startUris refs
    where
    startUris                   = [ "http://www.fh-wedel.de/~si/index.html"
				  ]
    refs                        = simpleFollowRef'
                                  [ v1 ++ ".*[.](html|pdf)" ]
                                  ( map (v1 ++) ["welcome[.]html"
						, "handouts/.*.html"
						, ".*[?]VAR=0"
						, "(.*/)?exec[.]html[?].*"
						, ".*/download[a-zA-Z0-9]*[.]html[?].*SRC=.*"
						]
				  )
                                  where
				  v1 = "http://www[.]fh-wedel[.]de/~si/(seminare|klausuren|praktika|projekte|termine|zettelkasten|vorlesungen/(c|cb|fp|internet|java|softwaredesign))/"

-- ------------------------------------------------------------

getOptions                      :: [String] -> (Maybe String, String, String)
getOptions ("-r":fn:as)         = (Just fn, s, r)
                                  where
                                  (_, s, r) = getOptions as
getOptions (uri : out : _)      = (Nothing, uri, out)
getOptions (uri : _)            = (Nothing, uri, "")
getOptions []                   = (Nothing, "", "")

main                            :: IO ()
main                            = do
                                  (resume, _sid, out) <- getArgs >>= return . getOptions
                                  runX ( hxtSetTraceAndErrorLogger NOTICE
                                         >>>
                                         arrIO0 (siCacher resume)
                                         >>>
                                         traceMsg 0 (unwords ["writing cache into XML file", out])
                                         >>>
                                         xpickleDocument xpickle [(a_indent, v_1)] out
                                         >>>
                                         traceMsg 0 "writing cache finished"

                                       )
                                    >> return ()

-- ------------------------------------------------------------
