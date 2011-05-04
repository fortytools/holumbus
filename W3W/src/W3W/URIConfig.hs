{-# OPTIONS #-}

-- ------------------------------------------------------------

module W3W.URIConfig
    ( w3wStart
    , w3wRefs

    , fileName
    , htmlFiles
    )
where

import           Data.List

import           Holumbus.Crawler

-- ------------------------------------------------------------

w3wStart                        :: [URI]
w3wStart                        = fhwStart

w3wRefs                         :: URI -> Bool
w3wRefs                         = fhwRefs

fhwHome                         :: String
fhwHome                         = "http://www.fh-wedel.de/"

ptlHome                         :: String
ptlHome                         = "http://www.ptl.de/"

fhwURIs                         :: [URI] -> [URI]
fhwURIs                         = map (fhwHome ++)

ptlURIs                         :: [URI] -> [URI]
ptlURIs                         = map (ptlHome ++)

fhwStart                        :: [URI]
fhwStart                        =  fhwURIs [ ""                 -- fhw start page
                                           , "~eg/"             -- Martin Egge's home
                                           , "~si/"             -- si's home
                                           ]
                                   ++
                                   ptlURIs [ ""                 -- ptl start page
                                           ]
                              

fhwRefs                         :: URI -> Bool
fhwRefs                         = simpleFollowRef'
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
                                                     ]
                                            , "~eg/" ++ htmlPaths       -- Martin Egges pages
                                            ]
                                  , ptlHome ++
                                            alternatives
                                            [ ".*/news/" ++ htmlPaths   -- for test: the ptl news pages added
                                            ]
                                  ]
                                  ( [ ".*[?].*"                         -- no URIs with parameters
                                    ]
                                  )

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
htmlFiles                       = optional (fileName ++ ext "html")

htmlPaths                       :: String
htmlPaths                       = filePath ++ htmlFiles

-- ------------------------------------------------------------

