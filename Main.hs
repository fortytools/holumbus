{-# OPTIONS -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances -XRank2Types #-}

-- ------------------------------------------------------------

module Main
where

import           Control.Applicative

import 		 Control.Monad.Reader
import		 Control.Monad.State
import 		 Control.Monad.ReaderStateIO

import           Data.Binary			( Binary )
import qualified Data.Binary			as B			-- else naming conflict with put and get from Monad.State

import           Data.List
import		 Data.Maybe

-- import qualified Data.Map       		as M
import qualified Data.Set       		as S

import           Holumbus.Index.Common		( writeToBinFile
						, loadFromBinFile
						)
import           Holumbus.Crawler.Robots
import           Holumbus.Index.Common          ( URI )

import		 Text.XML.HXT.Arrow
import		 Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch
						( match )

-- ------------------------------------------------------------

type URIs		= S.Set URI

data CrawlerConfig a	= CrawlerConfig
                          { cc_name		:: String
			  , cc_readAttributes	:: Attributes
			  , cc_readTimeOut	:: Int
			  , cc_preRefsFilter	:: ArrowXml a' => a' XmlTree XmlTree	-- -XRank2Types
			  , cc_processRefs	:: ArrowXml a' => a' XmlTree [URI]
			  , cc_preDocFilter     :: ArrowXml a' => a' XmlTree XmlTree
			  , cc_processDoc	:: ArrowXml a' => a' XmlTree a
			  , cc_followRef	:: URI -> Bool
			  }

data CrawlerState	= CrawlerState
                          { cs_toBeProcessed    :: URIs
			  , cs_wereProcessed    :: URIs
			  , cs_robots		:: Robots		-- is part of the state, it will grow during crawling
			  }

type CrawlAction c a	= ReaderStateIO (CrawlerConfig c) CrawlerState a

-- ------------------------------------------------------------

-- a rather boring default crawler configuration

defaultCrawlerConfig	:: CrawlerConfig a
defaultCrawlerConfig	= CrawlerConfig
                          { cc_name		= "Holumbus Crawler 0.0.1"
			  , cc_readAttributes	= []
			  , cc_readTimeOut	= 10000
			  , cc_preRefsFilter	= this			-- no preprocessing for refs extraction
			  , cc_processRefs	= listA none		-- don't extract refs
			  , cc_preDocFilter     = this			-- no document preprocessing
			  , cc_processDoc	= none			-- no document processing at all
			  , cc_followRef	= const False		-- do not follow any refs
			  }

-- ------------------------------------------------------------

instance Binary CrawlerState where
    put	s		= B.put (cs_toBeProcessed s)
			  >>
			  B.put (cs_wereProcessed s)
			  >>
			  B.put (cs_robots s)
    get			= do
			  tbp <- B.get
			  alp <- B.get
			  rbt <- B.get
			  return $ CrawlerState
				   { cs_toBeProcessed = tbp
				   , cs_wereProcessed = alp
				   , cs_robots        = rbt
				   }

putCrawlerState		:: CrawlerState	-> B.Put
putCrawlerState		= B.put

getCrawlerState		:: B.Get CrawlerState
getCrawlerState		= B.get

initCrawlerState	:: CrawlerState
initCrawlerState	= CrawlerState
			  { cs_toBeProcessed    = emptyURIs
			  , cs_wereProcessed    = emptyURIs
			  , cs_robots		= emptyRobots
			  }

uriProcessed		:: URI -> CrawlerState -> CrawlerState
uriProcessed uri s	= s { cs_toBeProcessed = deleteURI uri (cs_toBeProcessed s)
			    , cs_wereProcessed = insertURI uri (cs_wereProcessed s)
			    }

uriToBeProcessed	:: URI -> CrawlerState -> CrawlerState
uriToBeProcessed uri s
    | alreadyProcessed	= s
    | otherwise		= s { cs_toBeProcessed = insertURI uri (cs_toBeProcessed s) }
    where
    alreadyProcessed	= uri `S.member` (cs_wereProcessed s)

urisToBeProcessed	:: [URI] -> CrawlerState -> CrawlerState
urisToBeProcessed uris	s
			= foldl' (flip uriToBeProcessed) s uris

-- ------------------------------------------------------------

emptyURIs		:: URIs
emptyURIs		= S.empty

insertURI		:: URI -> URIs	-> URIs
insertURI		= S.insert

deleteURI		:: URI -> URIs	-> URIs
deleteURI		= S.delete

-- ------------------------------------------------------------

saveCrawlerState	:: FilePath -> CrawlAction c ()
saveCrawlerState fn	= do
			  s <- get
			  liftIO $ writeToBinFile fn s

loadCrawlerState	:: FilePath -> CrawlAction c ()
loadCrawlerState fn	= do
			  s <- liftIO $ loadFromBinFile fn
			  put s

crawlDoc		:: URI -> CrawlAction c (Maybe c)
crawlDoc uri		= do
			  modify (uriProcessed uri)
			  (uris, res) <- processDoc uri
			  mapM (modify . uriToBeProcessed) uris
			  return res

processDoc		:: URI -> CrawlAction c ([URI], Maybe c)
processDoc uri		= do
			  conf <- ask
			  [(uris, res)] <- liftIO $ runX (processDocArrow conf uri)
			  return ( filter (cc_followRef conf) uris
				 , listToMaybe res
				 )

processDocArrow		:: CrawlerConfig c -> URI -> IOSArrow a ([URI],[c])
processDocArrow c uri	= ( readDocument (cc_readAttributes c) uri
			    >>>
			    undefined
			  )
			  `withDefault` ([], [])

-- ------------------------------------------------------------

simpleFollowRef		:: (String -> Bool) -> (String -> Bool) -> (String -> Bool)
simpleFollowRef isAllowed isDenied
    			= isAllowed .&&. (not . isDenied)
			  where
			  (.&&.) = liftA2 (&&)

simpleFollowRef'	:: [String] -> [String] -> (String -> Bool)
simpleFollowRef' allowed denied
			= simpleFollowRef (match $ mkAlt allowed) (match $ mkAlt denied)
    where
    mkAlt		:: [String] -> String
    mkAlt rs		= "(" ++ intercalate "|" rs ++ ")"


-- ------------------------------------------------------------
{-
c1	= do
	  CCF e <- ask
	  modify (\ (CST s) -> CST (s ++ e ++ reverse s))
	  s1 <- get
	  return (e ++ state s1)

a1	= runReaderStateIO c1 (CCF "abc") (CST "xyz")
-}
-- ------------------------------------------------------------
