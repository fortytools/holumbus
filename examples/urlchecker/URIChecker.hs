{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
where

import           URIChecker.Check

-- ------------------------------------------------------------

-- | the list of document trees to be checked

sessions		:: [(String, URIClassList)]
sessions		= [ ( "http://localhost/~si/"
			    , [ ("http://localhost/~si/.*/welcome[.]html", 		Manual)
			      , ("http://localhost/~si/index*[.]html", 			Contents)
			      , ("http://localhost/~si.*[.]html[?].*",      		Manual)
			      , ("http://localhost/~si.*[.]html",			Exists)
			      , ("http://localhost/~si/.*[.](gif|jpg|css|ico|pdf)",	Exists)
			      , ("http://localhost/.*", 				Manual)
			      ] ++ defaults
			    )
			  , ( "http://localhost/~si/vorlesungen/java/java.html"
			    , [ ("http://localhost/~si/vorlesungen/java/welcome.html",		Ignore)
			      , ("http://localhost/~si/vorlesungen/java/.*/exec.html[?].*",	Exists)
			      , ("http://localhost/~si/vorlesungen/java/.*/downloadhtml.html[?].*[.]html",	Exists)
			      , ("http://localhost/~si/vorlesungen/java/.*",			Contents)
			      , ("http://localhost/~si/.*",					Exists)
			      , ("http://localhost/",						Ignore)
			      , ("http://www.fh-wedel.de/~si/",					Exists)
			      , ("http://.*/.*[.]html",						Exists)
			      ] ++ defaults
			    )
			  ]

-- | default uri handling
defaults		:: URIClassList
defaults		= [ ("http:.*",			Manual)
			  , ("https:.*", 		Manual)
			  , ("mailto:si@fh-wedel.de",	Ignore)
			  , ("mailto:.*", 		Manual)
			  , ("ftp:",			Manual)
			  , ("javascript:.*", 		Ignore)
			  , ("file:///.*", 		Illegal)
			  , ( ".*",			Illegal)
			  ]

-- ------------------------------------------------------------

main			:: IO ()
main			= main1 sessions

-- ------------------------------------------------------------
