-- ------------------------------------------------------------

module Holumbus.Crawler.Robots
where

import qualified Data.Map       		as M

import           Holumbus.Index.Common		( URI )

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


