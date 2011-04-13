{-# OPTIONS #-}

-- ------------------------------------------------------------

module WWWCheck.SimpleURIChecker
    ( main1
    , URIClass(..)
    , URIClassList
    )
where

import           Codec.Compression.BZip (compress, decompress)

import           Holumbus.Crawler

import           System.IO

import           Text.XML.HXT.Core
import           Text.XML.HXT.Cache
import           Text.XML.HXT.Curl

import           WWWCheck.OutputTemplate
import           WWWCheck.URIChecker

-- ------------------------------------------------------------

getOptions              :: [String] -> (Maybe String, String, String, String)
getOptions ("-r":fn:as) = (Just fn, s, r, tb)
                          where
                          (_, s, r, tb) = getOptions as
getOptions (uri : out : tb : _)
                        = (Nothing, uri, out, tb)
getOptions (uri : out : _)
                        = (Nothing, uri, out, "")
getOptions (uri : _)    = (Nothing, uri, "", "")
getOptions []           = (Nothing, "", "", "")

main1                   :: [String] -> [(String, URIClassList)] -> IO ()
main1 args sessList     = do
                          let (resume, sid, out, tb) = getOptions args
                          case lookup sid sessList of
                            Nothing -> hPutStrLn stderr $ unlines $
                                         [ unwords ["no URI config for start URI found:", show sid]
                                         , ""
                                         , "possible start uris are"
                                         , ""
                                         ]
                                         ++ map fst sessList

                            Just uris -> runX ( hxtSetTraceAndErrorLogger DEBUG
                                                >>>
                                                arrIO0 (simpleURIChecker resume sid uris)
                                                >>>
                                                genResultPage (if null tb then "." else tb) out sid uris
                                              )
                                         >> return ()

-- ------------------------------------------------------------

simpleURIChecker        :: Maybe String -> URI -> URIClassList -> IO DocMap
simpleURIChecker        = stdURIChecker
                          ( 10000                                               -- limit total number of documents
                          , 100                                                 -- # of documents analysed in parallel
                          , 1                                                   -- # of thread running in parallel
                          )
                          ( 500                                                 -- save intermediate state every 500 documents
                          , "./tmp/uri-check-"                                  -- path prefix for saving intermediate states
                          )
                          ( NOTICE                                              -- set trace level to 1
                          , NOTICE                                              -- set trace level for hxt to 1
                          )
                          "WWWCheck/1.0"                                        -- user agent
                          ( foldl (.) id $
                            [ withCache "./cache" (60 * 3 * 1 * 1) False        -- set cache dir, cache remains valid 3 minutes, 404 pages are cached
                            , withCompression (compress, decompress)            -- compress cache files
                            , withStrictDeserialize yes                         -- strict input of cache files
                            , withAcceptedMimeTypes [ text_html
                                                    , application_xhtml
                                                    ]
                            , withCurl [ (curl_max_filesize,    "2500000"       )       -- limit document size to 1 Mbyte
                                       , ("--max-time",         "60000"         )       -- limit maximum time for reading a document to 1 minute
                                       , ("--connect-timeout",  "10000"         )       -- limit connection time to 10 seconds
                                       , (curl_location,        v_0             )       -- do not automatically follow redirects
                                       , (curl_max_redirects,   "0"             )       -- no redirects
                                       ]
                            , withRedirect no
                            , withParseHTML no
                            , withParseByMimeType yes
                            , withWarnings no
                            , withEncodingErrors no
                            ]
                          )

-- ------------------------------------------------------------
