-- ------------------------------------------------------------

module Holumbus.Crawler.Robots
where

import           Control.DeepSeq

import           Data.Function.Selector

import           Data.List
import qualified Data.Map.Strict             as M
import           Data.Maybe

import           Holumbus.Crawler.Logger
import           Holumbus.Crawler.RobotTypes
import           Holumbus.Crawler.Types
import           Holumbus.Crawler.URIs

import qualified Network.URI                 as N

import           Text.XML.HXT.Cache
import           Text.XML.HXT.Core

{-
import           Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch

import qualified Debug.Trace as D
-}

-- ------------------------------------------------------------

-- | Add a robots.txt description for a given URI, if it's not already there.
-- The 1. main function of this module

robotsAddHost           :: CrawlerConfig a r -> AddRobotsAction
robotsAddHost conf uri rdm
    | not (isRobotsScheme uri)
                        = return rdm
    | isJust spec       = return rdm
    | otherwise         = do
                          (h, r) <- robotsGetSpec conf host
                          let rdm' = M.insert h r rdm
                          return $! rdm'
    where
    host                = getHost uri
    spec                = M.lookup host rdm

-- ------------------------------------------------------------

robotsDontAddHost       :: CrawlerConfig a r -> AddRobotsAction
robotsDontAddHost       = const $ const return

-- ------------------------------------------------------------

-- | Check whether a robot is not allowed to access a page.
-- The 2. main function of this module

robotsDisallow          :: Robots -> URI -> Bool
robotsDisallow rdm uri
    | not (isRobotsScheme uri)
                        = False
    | isNothing restr   = False
    | otherwise         = evalRestr $ fromJust restr
    where
    host                = getHost uri
    path'               = getURIPart N.uriPath uri
    restr               = M.lookup host rdm
    evalRestr           = foldr isDis False
                          where
                          isDis (r, a) v
                              | r `isPrefixOf` path'
                                &&
                                not (null r)            = a == Disallow
                              | otherwise               = v

-- ------------------------------------------------------------

getURIPart              :: (N.URI -> String) -> URI -> String
getURIPart f            = maybe "" f
                          .
                          N.parseURIReference

-- | Get the protocol-host-port part of an URI

getHost                 :: URI -> URI
getHost                 = getURIPart h
                          where
                          h u = show $ u { N.uriPath = ""
                                         , N.uriQuery = ""
                                         , N.uriFragment = ""
                                         }

isRobotsScheme          :: URI -> Bool
isRobotsScheme          = (`elem` ["http:", "https:"]) . getURIPart N.uriScheme

-- ------------------------------------------------------------

-- | Access, parse and evaluate a robots.txt file for a given URI

robotsGetSpec           :: CrawlerConfig a r -> URI -> IO (URI, RobotRestriction)
robotsGetSpec conf uri
    | not (isRobotsScheme uri)
                        = return ("", [])
    | null host         = return ("", [])
    | otherwise         = do
                          r <- getRobotsTxt conf host
                          s <- return $ evalRobotsTxt agent r
                          rnf s `seq` return (host, s)
    where
    host                = getHost uri
    agent               = getS theCrawlerName $ conf

-- ------------------------------------------------------------

-- | Try to get the robots.txt file for a given host.
-- If it's not there or any errors occur during access, the empty string is returned

getRobotsTxt            :: CrawlerConfig c r -> URI -> IO String
getRobotsTxt c uri      = runX processRobotsTxt >>= (return . concat)
    where
    processRobotsTxt    =  hxtSetTraceAndErrorLogger (getS theTraceLevelHxt c)
                           >>>
                           readDocument
                             [ getS theSysConfig c
                               >>>
                               withParseByMimeType       yes         -- these 3 options are important for reading none XML/HTML documents
                               >>>
                               withIgnoreNoneXmlContents no
                               >>>
                               withAcceptedMimeTypes    [text_plain] -- robots.txt is plain text
                               >>>
                               withRedirect              yes         -- follow redirects for robots.txt
                               >>>
                               withoutCache
                             ] (getHost uri ++ "/robots.txt")
                           >>>
                           documentStatusOk
                           >>>
                           getChildren
                           >>>
                           getText

-- ------------------------------------------------------------

-- | Parse the robots.txt, select the crawler specific parts and build a robots restriction value

evalRobotsTxt           :: String -> String -> RobotRestriction
evalRobotsTxt agent t   = lines
                          >>>
                          map (takeWhile (/= '#') >>> stringTrim)               -- remove comments and whitespace
                          >>>
                          filter (not . null)
                          >>>
                          filter ( stringToLower
                                   >>>
                                   takeWhile (/= ':')
                                   >>>
                                   (`elem` [ "disallow"
                                           , "allow"
                                           , "user-agent"
                                           , "crawl-delay"
                                           , "request-rate"
                                           , "visit-time"
                                           , "sitemap"
                                           ]
                                   )
                                 )
                          >>>
                          map ( span (/= ':')
                                >>>
                                ( stringToLower *** (drop 1 >>> stringTrim) )
                              )
                          >>>
                          dropWhile ( \ (x, y) ->
                                          ( x /= "user-agent"
                                            ||
                                            ( y /= "*" && not (y `isPrefixOf` agent) )
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
    toRestr ("disallow", uri)   = [(uri, Disallow)]     -- other directives are currently ignored
    toRestr ("allow",    uri)   = [(uri, Allow)]
    toRestr _                   = []

-- ------------------------------------------------------------

-- | Enable the evaluation of robots.txt

enableRobotsTxt         :: CrawlerConfig a r -> CrawlerConfig a r
enableRobotsTxt         = setS theAddRobotsAction robotsAddHost

-- | Disable the evaluation of robots.txt

disableRobotsTxt        :: CrawlerConfig a r -> CrawlerConfig a r
disableRobotsTxt        = setS theAddRobotsAction robotsDontAddHost

-- ------------------------------------------------------------
