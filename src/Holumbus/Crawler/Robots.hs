-- ------------------------------------------------------------

module Holumbus.Crawler.Robots
where

import           Data.Char
import qualified Data.Map       		as M

import           Holumbus.Index.Common		( URI )

import           Text.XML.HXT.Arrow

-- ------------------------------------------------------------

type Robots		= M.Map URI RobotRestriction
type RobotRestriction	= ()						-- TODO

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

