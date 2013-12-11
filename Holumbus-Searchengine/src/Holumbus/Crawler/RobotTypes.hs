-- ------------------------------------------------------------

module Holumbus.Crawler.RobotTypes
where

import           Control.DeepSeq

import           Data.Binary           (Binary)
import qualified Data.Binary           as B
import           Data.Char
import qualified Data.Map.Strict       as M

import           Holumbus.Crawler.URIs

import           Text.XML.HXT.Core

{-
import           Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch

import qualified Debug.Trace as D
-}

-- ------------------------------------------------------------

type Robots             = M.Map URI RobotRestriction
type RobotRestriction   = [RobotSpec]
type RobotSpec          = (URI, RobotAction)

data RobotAction        = Disallow | Allow
                          deriving (Eq, Show, Read, Enum)

type AddRobotsAction    = URI -> Robots -> IO Robots

-- ------------------------------------------------------------

instance Binary RobotAction where
    put                 = B.put . fromEnum
    get                 = B.get >>= return . toEnum

instance NFData RobotAction where

instance XmlPickler RobotAction where
    xpickle             = xpPrim

xpRobots                :: PU Robots
xpRobots                = xpElem "robots" $
                          xpMap "robot" "host" xpText xpRobotRestriction

xpRobotRestriction      :: PU RobotRestriction
xpRobotRestriction      = xpList $
                          xpElem "restriction" $
                          xpPair ( xpAttr "href"   $ xpText )
                                 ( xpAttr "access" $ xpickle )

-- ------------------------------------------------------------

emptyRobots             :: Robots
emptyRobots             = M.singleton "" []

robotsExtend            :: String -> AddRobotsAction
robotsExtend _robotName _uri robots
                        = return robots                 -- TODO

robotsIndex             :: URI -> Robots -> Bool
robotsIndex _uri _robots
                        = True                          -- TODO

robotsFollow            :: URI -> Robots -> Bool
robotsFollow _uri _robots
                        = True                          -- TODO

-- ------------------------------------------------------------

robotsNo        :: String -> LA XmlTree XmlTree
robotsNo what   = none
                  `when`
                  ( this
                    /> hasName "html"
                    /> hasName "head"
                    /> hasName "meta" -- getByPath ["html", "head", "meta"]
                    >>>
                    hasAttrValue "name" ( map toUpper
                                          >>>
                                          (== "ROBOTS")
                                        )
                    >>>
                    getAttrValue0 "content"
                    >>>
                    isA ( map (toUpper
                               >>>
                               (\ x -> if isLetter x then x else ' ')
                              )
                          >>>
                          words
                          >>>
                          (what `elem`)
                        )
                  )

-- | robots no index filter. This filter checks HTML documents
-- for a \<meta name=\"robots\" content=\"noindex\"\> in the head of the document

robotsNoIndex   :: ArrowXml a => a XmlTree XmlTree
robotsNoIndex   = fromLA $ robotsNo "NOINDEX"

-- | robots no follow filter. This filter checks HTML documents
-- for a \<meta name=\"robots\" content=\"nofollow\"\> in the head of the document

robotsNoFollow  :: ArrowXml a => a XmlTree XmlTree
robotsNoFollow  = fromLA $ robotsNo "NOFOLLOW"

-- ------------------------------------------------------------
