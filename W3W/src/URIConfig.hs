{-# OPTIONS #-}

-- ------------------------------------------------------------

module URIConfig
    ( UriConfig (..)
    , w3wStart
    , w3wRefs
    , fileName
    , htmlFiles
    , editTilde
    )
where

import           Data.List

import           Holumbus.Crawler

-- ------------------------------------------------------------

data UriConfig                  = UCTestIndex | UCFullIndex | UCDebugIndex
                                  deriving (Eq, Show)

w3wStart                        :: UriConfig -> [URI]
w3wStart                        = fhwStart

w3wRefs                         :: UriConfig -> URI -> Bool
w3wRefs                         = fhwRefs

fhwHome                         :: String
fhwHome                         = "http://www.fh-wedel.de/"

ptlHome                         :: String
ptlHome                         = "http://www.ptl.de/"

cgHome                          :: String
cgHome                          = "http://cg.fh-wedel.de/"

fhwURIs                         :: [URI] -> [URI]
fhwURIs                         = map (fhwHome ++)

ptlURIs                         :: [URI] -> [URI]
ptlURIs                         = map (ptlHome ++)

fhwStart                        :: UriConfig -> [URI]
fhwStart UCFullIndex
    =  fhwURIs
       [ ""                 -- fhw start page
       ]
       ++
       ptlURIs
       [ ""                 -- ptl start page
       ]

fhwStart UCTestIndex
    =  fhwURIs
       [ ""                 -- fhw start page
       , "~eg/"             -- Martin Egge's home
       , "~si/"             -- si's home
       , "mitarbeiter/wol/" -- wol's home
       , "online-campus/termine/aktuelles-semester/"
       ]
       ++
       ptlURIs
       [ ""                 -- ptl start page
       ]

fhwStart UCDebugIndex
    = [ "http://www.ptl.de/~ki/splan/automation/Vortrag.pdf"
      ]

fhwRefs :: UriConfig -> URI -> Bool
fhwRefs UCFullIndex
    = simpleFollowRef'
      [ fhwHome ++
                alternatives
                [ htmlPaths
                ]
      , ptlHome ++
                alternatives
                [ htmlPaths
                ]
      , cgHome ++
               alternatives
               [ htmlPaths
               ]
      ]
      ( [ ".*" ++
               alternatives
               [ "[?]"                                  -- no URIs with parameters
               , "/javadoc/"                            -- no javadoc
               ]
               ++ ".*"
        , fhwHome ++
                  alternatives
                  [ "~si/vorlesungen/.*/welcome.html"   -- no welcome pages from si
                  , "~si/vortraege/.*"                  -- no old talks from si
                  , "archiv/wol/htmlbaum/.*"            -- no junk from wol
                  ]
        ]
      )

fhwRefs UCTestIndex
    = simpleFollowRef'
      [ fhwHome ++
                alternatives
                [ ""                        -- the homepage
                , htmlFiles                 -- all top level fhw pages
                , "~si/" ++                 -- si's pages with dates
                         alternatives
                         [ "termine/" ++ htmlFiles
                         , "praktika/SoftwarePraktikum/index.html"
                         , "praktika/SoftwarePraktikum/20[1-9][0-9][sw]s/index.html"
                         , "seminare/[sw]s[0-9][0-9]/Termine/" ++ htmlFiles
                         , "vorlesungen/internet/" ++ htmlPaths
                         ]
                , "~eg/" ++ htmlPaths       -- Martin Egges pages
                , "mitarbeiter/wol/" ++ htmlPaths
                , "online-campus/termine/aktuelles-semester/"  ++ htmlPaths -- kalender
                ]
      , ptlHome ++
                alternatives
                [ ".*/news/" ++ htmlPaths   -- for test: the ptl news pages added
                ]
      ]
      ( [ ".*([?]|/javadoc/).*"                         -- no URIs with parameters, no javadoc
        , fhwHome ++ "~si/vorlesungen/.*/welcome.html"  -- no welcome pages from si
        ]
      )

fhwRefs UCDebugIndex
    = simpleFollowRef'
      [ fhwHome ++ "http://www.fh-wedel.de/mitarbeiter/ehemalige/her/"
      , "http://www.ptl.de/~ki/splan/automation/Vortrag.pdf"
      ]
      ([])

-- ------------------------------------------------------------

editTilde                       :: String -> String
editTilde                       = sed (("/~" ++) . drop 4) "/%7[Ee]"

-- ------------------------------------------------------------

-- common R.E.s

alternatives                    :: [String] -> String
alternatives                    = ("(" ++) . (++ ")") . intercalate "|"

optional                        :: String -> String
optional                        = ("(" ++) . (++ ")?")

ext                             :: String -> String
ext                             = ("[.]" ++)

fileName                        :: String
fileName                        = "[^/?]+"

filePath                        :: String
filePath                        = "(" ++ fileName ++ "/)*"

htmlFiles                       :: String
htmlFiles                       = optional (fileName ++ alternatives [ext "html", ext "htm", ext "pdf"])

htmlPaths                       :: String
htmlPaths                       = filePath ++ htmlFiles

-- ------------------------------------------------------------
