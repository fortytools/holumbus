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

			    -- java lecture
			  , ( "http://localhost/~si/vorlesungen/java/java.html"
			    , [ ("http://localhost/~si/vorlesungen/java/welcome.html",		Ignore)
			      , ("http://localhost/~si/vorlesungen/java/.*/exec.html[?].*",	Exists)
			      , ("http://localhost/~si/vorlesungen/java/.*/downloadhtml.html[?].*[.]html",	Exists)
			      , ("http://localhost/~si/vorlesungen/java/.*",			Contents)
			      , ("http://localhost/~si/.*",					Exists)
			      , ("http://localhost/",						Ignore)
			      , ("http://.*",							Exists)
			      ] ++ defaults
			    )
			  , ( "http://www.fh-wedel.de/~si/vorlesungen/java/java.html"
			    , [ ("http://www.fh-wedel.de/~si/vorlesungen/java/welcome.html",		Ignore)
			      , ("http://www.fh-wedel.de/~si/vorlesungen/java/.*[?]VAR=0",		Ignore)
			      , ("http://www.fh-wedel.de/~si/vorlesungen/java/.*/exec.html[?].*",	Illegal)
			      , ("http://www.fh-wedel.de/~si/vorlesungen/java/.*/exec.html[?].*",	Illegal)
			      , ("http://www.fh-wedel.de/~si/vorlesungen/java/.*/program.html[?].*[.]html",		Contents)
			      , ("http://www.fh-wedel.de/~si/vorlesungen/java/.*/download[a-zA-Z0-9]*.html[?].*SRC=.*",	Exists)
			      , ("http://www.fh-wedel.de/~si/vorlesungen/java/.*",			Contents)
			      , ("http://www.fh-wedel.de/~si/.*",					Exists)
			      , ("http://.*",								Exists)
			      ] ++ defaults
			    )
			  , ( "http://192.168.2.11/~si/vorlesungen/java/java.html"
			    , [ ("http://192.168.2.11/~si/vorlesungen/java/welcome.html",		Ignore)
			      , ("http://192.168.2.11/~si/vorlesungen/java/.*[?]VAR=0",			Ignore)
			      , ("http://192.168.2.11/~si/vorlesungen/java/.*/exec.html[?].*",		Illegal)
			      , ("http://192.168.2.11/~si/vorlesungen/java/.*/exec.html[?].*",		Illegal)
			      , ("http://192.168.2.11/~si/vorlesungen/java/.*/program.html[?].*[.]html",		Contents)
			      , ("http://192.168.2.11/~si/vorlesungen/java/.*/download[a-zA-Z0-9]*.html[?].*SRC=.*",	Exists)
			      , ("http://192.168.2.11/~si/vorlesungen/java/.*",				Contents)
			      , ("http://192.168.2.11/~si/.*",						Exists)
			      , ("http://.*",								Exists)
			      ] ++ defaults
			    )
			    -- FP lecture
			  , ( "http://www.fh-wedel.de/~si/vorlesungen/fp/fp.html"
			    , [ ("http://www.fh-wedel.de/~si/vorlesungen/fp/welcome.html",		Ignore)
			      , ("http://www.fh-wedel.de/~si/vorlesungen/fp/handouts/haskell.*",	Exists)
			      , ("http://www.fh-wedel.de/~si/vorlesungen/fp/.*[?]VAR=0",		Ignore)
			      , ("http://www.fh-wedel.de/~si/vorlesungen/fp/.*/exec.html[?].*",		Illegal)
			      , ("http://www.fh-wedel.de/~si/vorlesungen/fp/.*/exec.html[?].*",		Illegal)
			      , ("http://www.fh-wedel.de/~si/vorlesungen/fp/.*/program.html[?].*[.]html",		Contents)
			      , ("http://www.fh-wedel.de/~si/vorlesungen/fp/.*/download[a-zA-Z0-9]*.html[?].*SRC=.*",	Exists)
			      , ("http://www.fh-wedel.de/~si/vorlesungen/fp/.*",			Contents)
			      , ("http://www.fh-wedel.de/~si/.*",					Exists)
			      , ("http://.*",								Exists)
			      ] ++ defaults
			    )
			    -- hxt home
			  , ( "http://www.fh-wedel.de/~si/HXmlToolbox/hdoc/index.html"
			    , [ ("http://www.fh-wedel.de/~si/HXmlToolbox/hdoc/src/.*[.]html",		Exists)
			      , ("http://www.fh-wedel.de/~si/HXmlToolbox/hdoc/.*[.]html",		Contents)
			      , ("http://www.fh-wedel.de/usr/.*[.]html",				Ignore)		-- the default location for base documentation
			      , ("http://hackage.haskell.org/packages/archive/base/latest/doc/html/.*",	Exists)		-- the location on hackage for base
			      , ("http://.*",								Exists)
			      ] ++ defaults
			    )
			    -- holumbus site
			  , ( "http://holumbus.fh-wedel.de/"
			    , [ ("http://holumbus.fh-wedel.de/trac",					Contents)
			      , ("http://holumbus.fh-wedel.de.*",					Exists)
			      , ("http://darcs2.fh-wedel.de.*",						Exists)
			      , ("http://www.fh-wedel.de.*",						Exists)
			      , ("http://hackage.haskell.org.*",					Exists)
			      , ("http://www.haskell.org.*",						Exists)
			      , ("http://darcs.net.*",							Exists)
			      , ("http://www.opensource.org/licenses/mit-license.php",			Exists)
			      , ("http://.*.edgewall.org.*",						Exists)
			      ] ++ defaults
			    )
			  ]

-- | default uri handling
defaults		:: URIClassList
defaults		= [ ("http:.*",			Manual)
			  , ("https:.*", 		Manual)
			  , ("mailto:si@fh-wedel.de",	Ignore)
			  , ("mailto:.*", 		Manual)
			  , ("ftp:.*",			Manual)
			  , ("javascript:.*", 		Ignore)
			  , ("file:///.*", 		Illegal)
			  , ( ".*",			Illegal)
			  ]

-- ------------------------------------------------------------

main			:: IO ()
main			= main1 sessions

-- ------------------------------------------------------------
