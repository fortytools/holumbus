{-# OPTIONS #-}

-- ------------------------------------------------------------

module URIChecker.Check
    ( main1
    , URIClass(..)
    , URIClassList
    )
where

import           Holumbus.Crawler.URIChecker

import		 Data.Maybe

import           System.IO
import		 System.Environment

import		 Text.XML.HXT.Arrow

import		 URIChecker.Template

-- ------------------------------------------------------------

getOptions		:: [String] -> (Maybe String, String, String)
getOptions ("-r":fn:as)	= (Just fn, s, r)
			  where
			  (_, s, r) = getOptions as
getOptions (uri : out : _)
    			= (Nothing, uri, out)
getOptions (uri : _)	= (Nothing, uri, "")
getOptions []		= (Nothing, "", "")

main1			:: [(String, URIClassList)] -> IO ()
main1 sessList		= do
			  (resume, sid, out) <- getArgs >>= return . getOptions
			  let uris           = fromMaybe [] . lookup sid $ sessList
			  dm 		     <- simpleURIChecker resume sid uris
			  runX               $ genResultPage out sid uris dm
			  return             ()

-- ------------------------------------------------------------
