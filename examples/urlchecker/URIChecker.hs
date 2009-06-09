{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
where

import           Holumbus.Crawler.URIChecker

import           System.IO
import		 System.Environment
import		 Text.XML.HXT.Arrow ()

-- ------------------------------------------------------------

check_localhost_si 	:: (String, [(String, URIClass)])
check_localhost_si	= ( "http://localhost/~si/"
			  , [ ("http://localhost/~si/.*/welcome[.]html", 		Manual)
			    , ("http://localhost/~si/index*[.]html", 			Contents)
			    , ("http://localhost/~si.*[.]html[?].*",      		Manual)
			    , ("http://localhost/~si.*[.]html",				Exists)
			    , ("http://localhost/~si/.*[.](gif|jpg|css|ico|pdf)",	Exists)
			    , ("http://localhost/.*", 					Manual)
			    , ("http://.*", 						Manual)
			    , ("mailto:.*", 						Manual)
			    , ("https://.*", 						Manual)
			    , ("javascript:.*", 					Illegal)
			    , ("file:///.*", 						Illegal)
			    , ( ".*",							Illegal)
			    ]
			  )

-- ------------------------------------------------------------

getOptions		:: [String] -> (Maybe String, String)
getOptions ("-r":fn:as)	= (Just fn, r)
			  where
			  (_, r) = getOptions as
getOptions (out:_)	= (Nothing, out)
getOptions []		= (Nothing, "")

main	:: IO ()
main	= do
	  (resume, _out) <- getArgs >>= return . getOptions
	  dm 		<- uncurry (simpleURIChecker resume) check_localhost_si
	  putStrLn      $ show dm
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
