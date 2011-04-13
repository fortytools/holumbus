{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
where

import           Control.Arrow

import           WWWCheck.SimpleURIChecker

import           System.Environment

-- ------------------------------------------------------------

-- | the list of document trees to be checked

sessions,
  fhwSessions,
  fhw2Sessions,
  localhostSessions,
  hobelSessions,
  holumbusSessions      :: [(String, URIClassList)]

sessions                = map addDefaults $
                          fhwSessions ++ fhw2Sessions ++ localhostSessions ++ hobelSessions ++ holumbusSessions
                          where
                          addDefaults = second (++ defaults)

localhostSessions       = [ ( "http://localhost/~si/"
                            , [ ("http://localhost/~si/.*/welcome[.]html",              Manual)
                              , ("http://localhost/~si/index*[.]html",                  Contents)
                              , ("http://localhost/~si.*[.]html[?].*",                  Manual)
                              , ("http://localhost/~si.*[.]html",                       Exists)
                              , ("http://localhost/~si/.*[.](gif|jpg|css|ico|pdf)",     Exists)
                              , ("http://localhost/.*",                                 Manual)
                              ]
                            )

                            -- java lecture
                          , ( "http://localhost/~si/vorlesungen/java/java.html"
                            , [ ("http://localhost/~si/vorlesungen/java/welcome.html",          Ignore)
                              , ("http://localhost/~si/vorlesungen/java/.*/exec.html[?].*",     Manual)
                              , ("http://localhost/~si/vorlesungen/java/.*/downloadhtml.html[?].*[.]html",      Exists)
                              , ("http://localhost/~si/vorlesungen/java/.*",                    Contents)
                              , ("http://localhost/~si/.*",                                     Exists)
                              , ("http://localhost/",                                           Illegal)
                              , ("http://.*",                                                   Exists)
                              ]
                            )
                          ]

fhwSessions             = [ ( "http://www.fh-wedel.de/~si/vorlesungen/java/java.html"
                            , [ ("http://www.fh-wedel.de/~si/vorlesungen/java/welcome.html",            Ignore)
                              , ("http://www.fh-wedel.de/~si/vorlesungen/java/.*[?]VAR=0",              Ignore)
                              , ("http://www.fh-wedel.de/~si/vorlesungen/java/.*/exec.html[?].*",       Illegal)
                              , ("http://www.fh-wedel.de/~si/vorlesungen/java/.*/exec.html[?].*",       Illegal)
                              , ("http://www.fh-wedel.de/~si/vorlesungen/java/.*/program.html[?].*[.]html",             Contents)
                              , ("http://www.fh-wedel.de/~si/vorlesungen/java/.*/download[a-zA-Z0-9]*.html[?].*SRC=.*", Exists)
                              , ("http://www.fh-wedel.de/~si/vorlesungen/java/.*",                      Contents)
                              , ("http://www.fh-wedel.de/~si/.*",                                       Exists)
                              , ("http://.*",                                                           Exists)
                              ]
                            )
                            -- FP lecture
                          , ( "http://www.fh-wedel.de/~si/vorlesungen/fp/fp.html"
                            , [ ("http://www.fh-wedel.de/~si/vorlesungen/fp/welcome.html",              Ignore)
                              , ("http://www.fh-wedel.de/~si/vorlesungen/fp/handouts/haskell.*",        Exists)
                              , ("http://www.fh-wedel.de/~si/vorlesungen/fp/.*[?]VAR=0",                Ignore)
                              , ("http://www.fh-wedel.de/~si/vorlesungen/fp/.*/exec.html[?].*",         Illegal)
                              , ("http://www.fh-wedel.de/~si/vorlesungen/fp/.*/exec.html[?].*",         Illegal)
                              , ("http://www.fh-wedel.de/~si/vorlesungen/fp/.*/program.html[?].*[.]html",               Contents)
                              , ("http://www.fh-wedel.de/~si/vorlesungen/fp/.*/download[a-zA-Z0-9]*.html[?].*SRC=.*",   Exists)
                              , ("http://www.fh-wedel.de/~si/vorlesungen/fp/.*",                        Contents)
                              , ("http://www.fh-wedel.de/~si/.*",                                       Exists)
                              , ("http://.*",                                                           Exists)
                              ]
                            )
                            -- hxt home
                          , ( "http://www.fh-wedel.de/~si/HXmlToolbox/hdoc/index.html"
                            , [ ("http://www.fh-wedel.de/~si/HXmlToolbox/hdoc/src/.*[.]html",           Exists)
                              , ("http://www.fh-wedel.de/~si/HXmlToolbox/hdoc/.*[.]html",               Contents)
                              , ("http://www.fh-wedel.de/usr/.*[.]html",                                Ignore)         -- the default location for base documentation
                              , ("http://hackage.haskell.org/packages/archive/base/latest/doc/html/.*", Exists)         -- the location on hackage for base
                              , ("http://.*",                                                           Exists)
                              ]
                            )
                          ]

hobelSessions           = [ ( "http://192.168.2.11/~si/vorlesungen/java/java.html"
                            , [ ("http://192.168.2.11/~si/vorlesungen/java/welcome.html",               Ignore)
                              , ("http://192.168.2.11/~si/vorlesungen/java/.*[?]VAR=0",                 Ignore)
                              , ("http://192.168.2.11/~si/vorlesungen/java/.*/exec.html[?].*",          Illegal)
                              , ("http://192.168.2.11/~si/vorlesungen/java/.*/exec.html[?].*",          Illegal)
                              , ("http://192.168.2.11/~si/vorlesungen/java/.*/program.html[?].*[.]html",                Contents)
                              , ("http://192.168.2.11/~si/vorlesungen/java/.*/download[a-zA-Z0-9]*.html[?].*SRC=.*",    Exists)
                              , ("http://192.168.2.11/~si/vorlesungen/java/.*",                         Contents)
                              , ("http://192.168.2.11/~si/.*",                                          Exists)
                              , ("http://.*",                                                           Exists)
                              ]
                            )
                          ]

holumbusSessions        = [ ( "http://holumbus.fh-wedel.de/"
                            , [ ("http://holumbus.fh-wedel.de/trac",                                    Contents)
                              , ("http://holumbus.fh-wedel.de.*",                                       Exists)
                              , ("http://darcs2.fh-wedel.de.*",                                         Exists)
                              , ("http://www.fh-wedel.de.*",                                            Exists)
                              , ("http://hackage.haskell.org.*",                                        Exists)
                              , ("http://www.haskell.org.*",                                            Exists)
                              , ("http://darcs.net.*",                                                  Exists)
                              , ("http://www.opensource.org/licenses/mit-license.php",                  Exists)
                              , ("http://.*.edgewall.org.*",                                            Exists)
                              ]
                            )
                          ]

fhw2Sessions            = [ ( "http://www2.fh-wedel.de/"
                            , [ ("http://www2.fh-wedel.de/~splan/index.html[?].*",                      Ignore)         -- queries are not interesting
                              , ("http://www2.fh-wedel.de/~cwlan/.*",                                   Contents)       -- cwlan home pages
                              , ("http://www2.fh-wedel.de/~.*",                                         Exists)         -- personal home pages
                              , ("http://www2.fh-wedel.de/%7E.*",                                       Exists)         -- personal home pages
                              -- , ("http://biblserv.fh-wedel.de.*",                                    Manual)         -- library opac (contains errors)
                              , ("http://www2.fh-wedel.de/.*[?].*",                                     Exists)         -- pages with query string
                              , ("http://www2.fh-wedel.de(/.*)?",                                       Contents)       -- "real" pages
                              , ("http://.*",                                                           Exists)
                              ]
                            )
                          ]

-- | default uri handling
defaults                :: URIClassList
defaults                = [ ("http:.*",                 Manual)
                          , ("https:.*",                Manual)
                          , ("mailto:si@fh-wedel.de",   Ignore)
                          , ("mailto:uwe@fh-wedel.de",  Ignore)
                          , ("mailto:.*",               Manual)
                          , ("news:.*",                 Manual)
                          , ("ftp:.*",                  Manual)
                          , ("javascript:.*",           Ignore)
                          , ("file:///.*",              Illegal)
                          , ( ".*",                     Illegal)
                          ]

-- ------------------------------------------------------------

main                    :: IO ()
main                    = do
                          args <- getArgs
                          main1 args sessions

-- ------------------------------------------------------------
