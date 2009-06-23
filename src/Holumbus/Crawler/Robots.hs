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

import           Holumbus.Index.Common		( URI )

import 		 Network.URI 			hiding
    						( URI )
import qualified Network.URI			as N

import           Text.XML.HXT.Arrow
-- import		 Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch

-- import qualified Debug.Trace as D

-- ------------------------------------------------------------

type Robots		= M.Map URI RobotRestriction
type RobotRestriction	= [RobotSpec]						-- TODO
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

robotsDisallow		:: URI -> Robots -> Bool
robotsDisallow uri rdm
    | null host		= False
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

getURIPart		:: (N.URI -> String) -> URI -> String
getURIPart f		= maybe "" f
			  .
			  N.parseURIReference

getHost			:: URI -> URI
getHost			= getURIPart h
			  where
			  h u = show $ u { uriPath = ""
					 , uriQuery = ""
					 , uriFragment = ""
					 }

test1 = robotsGetSpec "HolIndex" "http://www.wikipedia.org/"
test2 = robotsGetSpec "DOC"      "http://www.wikipedia.org/"

robotsGetSpec		:: String -> URI -> IO (URI, RobotRestriction)
robotsGetSpec agent uri
    | null host		= return ("", [])
    | otherwise		= do
			  r <- getRobotsTxt host
			  return (host, evalRobotsTxt agent r)
    where
    host 		= getHost uri

getRobotsTxt		:: URI -> IO String
getRobotsTxt uri	= do
			  res <- runX ( readDocument [ (a_parse_by_mimetype, v_1)
						     , (a_trace, v_0)
						     , ("curl-L",v_1)
						     ] (getHost uri ++ "/robots.txt")
					>>>
					documentStatusOk
					>>>
					getChildren
					>>>
				        getText
				      )
			  return $ concat res

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
{-
matchDirective		:: String -> Bool
matchDirective		= match ("(" ++ intercalate "|" directives ++ ")" ++ ":")
			  where
			  directives = ["disallow", "allow", "user-agent", "crawl-delay", "request-rate", "visit-time", "sitemap"]

rbm = M.fromList [("http://localhost", [("/emil/egon.html",Disallow),("/emil/",Allow),("/",Disallow)])]
-}

-- ------------------------------------------------------------

emptyRobots		:: Robots
emptyRobots		= M.empty

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

