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
                                           ]
                                   ++
                                   ptlURIs [ ""                 -- ptl start page
                                           ]


fhwRefs                         :: URI -> Bool
fhwRefs                         = simpleFollowRef'
                                  [ fhwHome ++
                                            alternatives
                                            [ htmlPaths
                                            ]
                                  , ptlHome ++
                                            alternatives
                                            [ htmlPaths   -- for test: the ptl news pages added
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

