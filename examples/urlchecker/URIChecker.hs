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
			  ]

-- | default uri handling
defaults		:: URIClassList
defaults		= [ ("http:.*",		Manual)
			  , ("https:.*", 	Manual)
			  , ("mailto:.*", 	Manual)
			  , ("ftp:",		Manual)
			  , ("javascript:.*", 	Ignore)
			  , ("file:///.*", 	Illegal)
			  , ( ".*",		Illegal)
			  ]

-- ------------------------------------------------------------

main			:: IO ()
main			= main1 sessions

-- ------------------------------------------------------------
