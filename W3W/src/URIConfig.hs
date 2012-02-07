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
fhwStart UCFullIndex            =  fhwURIs [ ""                 -- fhw start page
                                           ]
                                   ++
                                   ptlURIs [ ""                 -- ptl start page
                                           ]

fhwStart UCTestIndex            =  fhwURIs [
                                           ""                 -- fhw start page
                                           , "~eg/"             -- Martin Egge's home
                                           , "~si/"             -- si's home
                                           , "online-campus/termine/aktuelles-semester/"
                                           ]
                                    ++
                                    ptlURIs [ ""                 -- ptl start page
                                            ]

fhwStart UCDebugIndex            =  fhwURIs [
                                             "mitarbeiter/ehemalige/her/"
                                            ]

fhwRefs                         :: UriConfig -> URI -> Bool
fhwRefs UCFullIndex             = simpleFollowRef'
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
                                  ( [ ".*[?].*"                 -- no URIs with parameters
                                    ]
                                  )
fhwRefs UCTestIndex             = simpleFollowRef'
                                  [ fhwHome ++
                                            alternatives
                                            [
                                             ""                        -- the homepage
                                             , htmlFiles                 -- all top level fhw pages
                                             , "~si/" ++                 -- si's pages with dates
                                                     alternatives
                                                     [ "termine/" ++ htmlFiles
                                                     , "praktika/SoftwarePraktikum/index.html"
                                                     , "praktika/SoftwarePraktikum/20[1-9][0-9][sw]s/index.html"
                                                     , "seminare/[sw]s[0-9][0-9]/Termine/" ++ htmlFiles
                                                     ]
                                            , "~eg/" ++ htmlPaths       -- Martin Egges pages
                                            , "online-campus/termine/aktuelles-semester/"  ++ htmlPaths -- kalender
                                            ]
                                  , ptlHome ++
                                            alternatives
                                            [ ".*/news/" ++ htmlPaths   -- for test: the ptl news pages added
                                            ]
                                  ]
                                  ( [ ".*[?].*"                         -- no URIs with parameters
                                    ]
                                  )

fhwRefs UCDebugIndex            = simpleFollowRef'
                                  [ fhwHome ++ "http://www.fh-wedel.de/mitarbeiter/ehemalige/her/"
                                  ]
                                  ([])

-- ------------------------------------------------------------

editTilde			:: String -> String
editTilde			= sed (("/~" ++) . drop 4) "/%7[Ee]"

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
