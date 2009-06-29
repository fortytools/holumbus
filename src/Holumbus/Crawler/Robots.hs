-- ------------------------------------------------------------

module Holumbus.Crawler.Robots
where

import		 Control.Parallel.Strategies

import           Data.Binary			( Binary )
import qualified Data.Binary			as B
import           Data.Char
import           Data.List
import qualified Data.Map       		as M
import		 Data.Maybe

import           Holumbus.Crawler.Constants
import           Holumbus.Crawler.URIs

import 		 Network.URI 			hiding	( URI )
import qualified Network.URI			as N

import           Text.XML.HXT.Arrow

{-
import		 Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch

import qualified Debug.Trace as D
-}

-- ------------------------------------------------------------

type Robots		= M.Map URI RobotRestriction
type RobotRestriction	= [RobotSpec]
type RobotSpec		= (URI, RobotAction)

data RobotAction	= Disallow | Allow
			  deriving (Eq, Show, Enum)

-- ------------------------------------------------------------

instance Binary RobotAction where
    put			= B.put . fromEnum
    get			= do
			  b <- B.get
			  return (toEnum b)

instance NFData RobotAction where
    rnf			= rwhnf

-- ------------------------------------------------------------

-- | Add a robots.txt description for a given URI, if it's not already there.
-- The 1. main function of this module

robotsAddHost		:: Attributes -> URI -> Robots -> IO Robots
robotsAddHost attrs uri rdm
    | isJust spec	= return rdm
    | otherwise		= do
			  (h, r) <- robotsGetSpec attrs host
			  return $! M.insert h r rdm
    where
    host		= getHost uri
    spec		= M.lookup host rdm

-- ------------------------------------------------------------

-- | Check whether a robot is not allowed to access a page.
-- The 2. main function of this module

robotsDisallow		:: Robots -> URI -> Bool
robotsDisallow rdm uri
    | isNothing restr	= False
    | otherwise		= evalRestr $ fromJust restr
    where
    host 		= getHost uri
    path'		= getURIPart uriPath uri
    restr		= M.lookup host rdm
    evalRestr		= foldr isDis False
			  where
			  isDis (r, a) v
			      | r `isPrefixOf` path'	= a == Disallow
			      | otherwise		= v

-- ------------------------------------------------------------

getURIPart		:: (N.URI -> String) -> URI -> String
getURIPart f		= maybe "" f
			  .
			  N.parseURIReference

-- | Get the protocol-host-port part of an URI

getHost			:: URI -> URI
getHost			= getURIPart h
			  where
			  h u = show $ u { uriPath = ""
					 , uriQuery = ""
					 , uriFragment = ""
					 }
-- ------------------------------------------------------------

-- | Access, parse and evaluate a robots.txt file for a given URI

robotsGetSpec		:: Attributes -> URI -> IO (URI, RobotRestriction)
robotsGetSpec attrs uri
    | null host		= return ("", [])
    | otherwise		= do
			  r <- getRobotsTxt attrs host
			  s <- return $ evalRobotsTxt agent r
			  rnf s `seq` return (host, s)
    where
    host 		= getHost uri
    agent		= fromMaybe defaultCrawlerName . lookup curl_user_agent $ attrs

-- ------------------------------------------------------------

-- | Try to get the robots.txt file for a given host.
-- If it's not there or any errors occuduring acces, the empty string is returned

getRobotsTxt		:: Attributes -> URI -> IO String
getRobotsTxt attrs uri	= do
			  res <- runX ( readDocument ( addEntries [ (a_parse_by_mimetype,        v_1)	-- these 3 options are important for reading none XML/HTML documents
								  , (a_parse_html,               v_0)
								  , (a_ignore_none_xml_contents, v_0)
								  , (a_accept_mimetypes, "text/plain")	-- robots.txt is plain text
								  , (curl_location,              v_1)	-- follow redirects for robots.txt
								  ]
						                  attrs
						     ) (getHost uri ++ "/robots.txt")
					>>>
					documentStatusOk
					>>>
					getChildren
					>>>
				        getText
				      )
			  return $ concat res

-- ------------------------------------------------------------

-- | Parse the robots.txt, select the crawler specific parts and build a robots restriction value

evalRobotsTxt		:: String -> String -> RobotRestriction
evalRobotsTxt agent t	= lines
			  >>>
			  map (takeWhile (/= '#') >>> stringTrim)		-- remove comments and whitespace
			  >>>
			  filter (not . null)
			  >>>
			  filter ( stringToLower
				   >>>
				   takeWhile (/= ':')
				   >>>
				   (`elem` ["disallow", "allow", "user-agent", "crawl-delay", "request-rate", "visit-time", "sitemap"])
				 )
			  >>>
			  map ( span (/= ':')
				>>>
				( stringToLower *** (drop 1 >>> stringTrim) )
			      )
			  >>>
			  dropWhile ( \ (x, y) -> ( x /= "user-agent"
						    ||
						    ( y /= "*" && y /= agent )
						  )
				    )
			  >>>
			  drop 1
			  >>>
			  takeWhile (fst >>> (/= "user-agent"))
			  >>>
			  concatMap toRestr
			  $
			  t
    where
    toRestr ("disallow", uri)	= [(uri, Disallow)]				-- other directives are currently ignored
    toRestr ("allow",    uri)	= [(uri, Allow)]
    toRestr _			= []

-- ------------------------------------------------------------

emptyRobots		:: Robots
emptyRobots		= M.singleton "" []

robotsExtend		:: String -> URI -> Robots -> IO Robots
robotsExtend _robotName _uri robots
			= return robots			-- TODO

robotsIndex		:: URI -> Robots -> Bool
robotsIndex _uri _robots
			= True				-- TODO

robotsFollow		:: URI -> Robots -> Bool
robotsFollow _uri _robots
			= True				-- TODO

-- ------------------------------------------------------------

robotsNo	:: String -> LA XmlTree XmlTree
robotsNo what	= none
		  `when`
		  ( this /> hasName "html" /> hasName "head" /> hasName "meta" -- getByPath ["html", "head", "meta"]
		    >>>
		    hasAttrValue "name" ( map toUpper
					  >>>
					  (== "ROBOTS")
					)
		    >>>
		    getAttrValue0 "content"
		    >>>
		    isA ( map (toUpper >>> (\ x -> if isLetter x then x else ' '))
			  >>>
			  words
			  >>>
			  (what `elem`)
			)
		  )	  

-- | robots no index filter. This filter checks HTML documents
-- for a \<meta name=\"robots\" content=\"noindex\"\> in the head of the document

robotsNoIndex	:: ArrowXml a => a XmlTree XmlTree
robotsNoIndex	= fromLA $ robotsNo "NOINDEX"

-- | robots no follow filter. This filter checks HTML documents
-- for a \<meta name=\"robots\" content=\"nofollow\"\> in the head of the document

robotsNoFollow	:: ArrowXml a => a XmlTree XmlTree
robotsNoFollow	= fromLA $ robotsNo "NOFOLLOW"

-- ------------------------------------------------------------

