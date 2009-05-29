{-# OPTIONS -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances -XRank2Types #-}

-- ------------------------------------------------------------

module Main
where

import           Control.Applicative

import 		 Control.Monad.Reader
import		 Control.Monad.State
import 		 Control.Monad.ReaderStateIO

-- import		 Control.Parallel.Strategies

import           Data.Binary			( Binary )
import qualified Data.Binary			as B			-- else naming conflict with put and get from Monad.State

import           Data.List
import		 Data.Maybe

import qualified Data.Map       		as M
import qualified Data.Set       		as S

import           Holumbus.Index.Common		( writeToBinFile
						, loadFromBinFile
						)
import           Holumbus.Crawler.Robots
import           Holumbus.Index.Common          ( URI )

import           System.IO

import		 Text.XML.HXT.Arrow		hiding ( when )
import qualified Text.XML.HXT.Arrow		as X
import		 Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch
						( match )

-- ------------------------------------------------------------

type URIs		= S.Set URI

data CrawlerConfig a r	= CrawlerConfig
                          { cc_name		:: ! String
			  , cc_readAttributes	:: ! Attributes
			  , cc_readTimeOut	:: ! Int
			  , cc_preRefsFilter	:: ArrowXml a' => a' XmlTree XmlTree	-- -XRank2Types
			  , cc_processRefs	:: ArrowXml a' => a' XmlTree URI
			  , cc_preDocFilter     :: ArrowXml a' => a' XmlTree XmlTree
			  , cc_processDoc	:: ArrowXml a' => a' XmlTree a
			  , cc_accumulate	:: (URI, a) -> r -> r
			  , cc_followRef	:: URI -> Bool
			  , cc_traceLevel	:: ! Int
			  }

data CrawlerState r	= CrawlerState
                          { cs_toBeProcessed    :: ! URIs
			  , cs_wereProcessed    :: ! URIs
			  , cs_robots		:: ! Robots		-- is part of the state, it will grow during crawling
			  , cs_resultAccu       :: r
			  }
			  deriving (Show)

type CrawlAction a r x	= ReaderStateIO (CrawlerConfig a r) (CrawlerState r) x

-- ------------------------------------------------------------

-- a rather boring default crawler configuration

defaultCrawlerConfig	:: ((URI, a) -> r -> r) -> CrawlerConfig a r
defaultCrawlerConfig op	= CrawlerConfig
                          { cc_name		= "Holumbus Crawler 0.0.1"
			  , cc_readAttributes	= []
			  , cc_readTimeOut	= 10000
			  , cc_preRefsFilter	= this			-- no preprocessing for refs extraction
			  , cc_processRefs	= none			-- don't extract refs
			  , cc_preDocFilter     = this			-- no document preprocessing
			  , cc_processDoc	= none			-- no document processing at all
			  , cc_accumulate	= op			-- combining function for result accumulating
			  , cc_followRef	= const False		-- do not follow any refs
			  , cc_traceLevel	= 1			-- traceLevel
			  }

-- ------------------------------------------------------------

instance (Binary r) => Binary (CrawlerState r) where
    put	s		= B.put (cs_toBeProcessed s)
			  >>
			  B.put (cs_wereProcessed s)
			  >>
			  B.put (cs_robots s)
			  >>
			  B.put (cs_resultAccu s)
    get			= do
			  tbp <- B.get
			  alp <- B.get
			  rbt <- B.get
			  acc <- B.get
			  return $ CrawlerState
				   { cs_toBeProcessed = tbp
				   , cs_wereProcessed = alp
				   , cs_robots        = rbt
				   , cs_resultAccu    = acc
				   }

putCrawlerState		:: (Binary r) => CrawlerState	r -> B.Put
putCrawlerState		= B.put

getCrawlerState		:: (Binary r) => B.Get (CrawlerState r)
getCrawlerState		= B.get

initCrawlerState	:: r -> CrawlerState r
initCrawlerState r	= CrawlerState
			  { cs_toBeProcessed    = emptyURIs
			  , cs_wereProcessed    = emptyURIs
			  , cs_robots		= emptyRobots
			  , cs_resultAccu	= r
			  }
{-
uriToBeProcessed	:: URI -> CrawlerState r -> CrawlerState r
uriToBeProcessed uri s
    | alreadyProcessed	= s
    | otherwise		= s { cs_toBeProcessed = insertURI uri (cs_toBeProcessed s) }
    where
    alreadyProcessed	= uri `S.member` (cs_wereProcessed s)

urisToBeProcessed	:: [URI] -> CrawlerState r -> CrawlerState r
urisToBeProcessed uris	s
			= foldl' (flip uriToBeProcessed) s uris
-}
-- ------------------------------------------------------------

emptyURIs		:: URIs
emptyURIs		= S.empty

insertURI		:: URI -> URIs	-> URIs
insertURI		= S.insert

deleteURI		:: URI -> URIs	-> URIs
deleteURI		= S.delete

-- ------------------------------------------------------------
--
-- basic crawler actions

traceCrawl		:: Int -> String -> CrawlAction c r ()
traceCrawl l msg		= do
			  l0 <- asks cc_traceLevel
			  when (l >= l0) $ liftIO $ hPutStrLn stderr $ "-" ++ "- (" ++ show l ++ ") " ++ msg

saveCrawlerState	:: (Binary r) => FilePath -> CrawlAction c r ()
saveCrawlerState fn	= do
			  s <- get
			  liftIO $ writeToBinFile fn s

loadCrawlerState	:: (Binary r) => FilePath -> CrawlAction c r ()
loadCrawlerState fn	= do
			  s <- liftIO $ loadFromBinFile fn
			  put s


uriProcessed		:: URI -> CrawlAction c r ()
uriProcessed uri	= modify uriProcessed'
    where
    uriProcessed' s	= s { cs_toBeProcessed = deleteURI uri (cs_toBeProcessed s)
			    , cs_wereProcessed = insertURI uri (cs_wereProcessed s)
			    }

uriToBeProcessed		:: URI -> CrawlAction c r ()
uriToBeProcessed uri		= modify uriToBeProcessed'
    where
    uriToBeProcessed' s
	| alreadyProcessed	= s
	| otherwise		= s { cs_toBeProcessed = insertURI uri (cs_toBeProcessed s) }
	where
	alreadyProcessed	= uri `S.member` (cs_wereProcessed s)


accumulateRes			:: (URI, c) -> CrawlAction c r ()
accumulateRes res		= do
				  combine <- asks cc_accumulate
				  modify (accumulate (combine res))
    where
    accumulate op s		= s { cs_resultAccu = op (cs_resultAccu s)}
			  
-- ------------------------------------------------------------

crawlerLoop		:: CrawlAction c r ()
crawlerLoop		= do
			  undefined

-- | crawl a single doc, mark doc as proessed, collect new hrefs and combine doc result with accumulator in state

crawlDoc		:: URI -> CrawlAction c r ()
crawlDoc uri		= do
			  traceCrawl 1 $ "crawlDoc: " ++ show uri
			  uriProcessed uri				-- uri is put into processed URIs
			  (uris, res) <- processDoc uri			-- get document and extract new refs and result

			  traceCrawl 1 $ "crawlDoc: new uris: " ++ show uris
			  mapM_ uriToBeProcessed uris			-- insert new uris into toBeProcessed set
			  maybe (return ()) accumulateRes res		-- combine result with state accu

processDoc		:: URI -> CrawlAction c r ([URI], Maybe (URI, c))
processDoc uri		= do
			  conf <- ask
			  [(uris, res)] <- liftIO $ runX (processDocArrow conf uri)
			  return ( filter (cc_followRef conf) uris
				 , listToMaybe res
				 )
-- | From a document two results are computed, 1. the list of all hrefs in the contents,
-- and 2. the collected info contained in the page. This result is augmented with the transfer uri
-- such that following functions know the source of this contents. The transfer-URI may be another one
-- as the input uri, there could happen a redirect in the http request.
--
-- The two listA arrows make the whole arrow deterministic, and it will never fail

processDocArrow		:: CrawlerConfig c r -> URI -> IOSArrow a ([URI], [(URI, c)])
processDocArrow c uri	= ( readDocument (cc_readAttributes c) uri
			    >>>
			    setTraceLevel (cc_traceLevel c)
			    >>>
			    ( listA ( cc_preRefsFilter c
				      >>>
				      cc_processRefs c
				    )
			      &&&
			      listA ( getAttrValue "transfer-URI"
				      &&&
				      ( cc_preDocFilter c
					>>>
					cc_processDoc c
				      )
				    )
			    )
			  )
			  `withDefault` ([], [])

-- ------------------------------------------------------------

runCrawler			:: CrawlAction c r x -> CrawlerConfig c r -> CrawlerState r -> IO (x, CrawlerState r)
runCrawler			= runReaderStateIO

-- run a crawler and deliver just the accumulated result value

execCrawler			:: CrawlAction c r x -> CrawlerConfig c r -> CrawlerState r -> IO r
execCrawler cmd config initState
				= do
				  (_, finalState) <- runCrawler cmd config initState
				  return (cs_resultAccu finalState)

-- ------------------------------------------------------------

simpleFollowRef			:: (String -> Bool) -> (String -> Bool) -> (String -> Bool)
simpleFollowRef isAllowed isDenied
    				= isAllowed .&&. (not . isDenied)
				  where
				  (.&&.) = liftA2 (&&)

simpleFollowRef'		:: [String] -> [String] -> (String -> Bool)
simpleFollowRef' allowed denied
				= simpleFollowRef (match $ mkAlt allowed) (match $ mkAlt denied)
    where
    mkAlt			:: [String] -> String
    mkAlt rs			= "(" ++ intercalate "|" rs ++ ")"


-- ------------------------------------------------------------

defaultHtmlCrawlerConfig	:: CrawlerConfig a r -> CrawlerConfig a r
defaultHtmlCrawlerConfig c	= c
				  { cc_readAttributes	= [ (a_validate,   		 v_0)
							  , (a_parse_html,		 v_1)
							  , (a_encoding,		 isoLatin1)
							  , (a_issue_warnings, 		 v_0)
							  , (a_ignore_none_xml_contents, v_1)
							  ]
				  , cc_preRefsFilter	= this
				  , cc_processRefs	= getHtmlReferences
				  }

-- ------------------------------------------------------------

getHtmlReferences 		:: ArrowXml a => a XmlTree URI
getHtmlReferences		= getRefs' $< computeDocBase
    where
    getRefs' base	        = fromLA $
				  deep (hasNameWith ((`elem` ["a","frame","iframe"]) . localPart))
				  >>>
				  ( getAttrValue0 "href"
				    <+>
				    getAttrValue0 "src"
				  )
				  >>^ (toAbsRef base)

toAbsRef        		:: URI -> URI -> URI
toAbsRef base ref		= ( expandURIString ref			-- here >>> is normal function composition
				    >>>
				    fromMaybe ref
				    >>>
				    removeFragment
				  ) base
    where
    removeFragment r
        | "#" `isPrefixOf` path = reverse . tail $ path
        | otherwise 		= r
        where
        path 			= dropWhile (/='#') . reverse $ r 

-- | Compute the base of a webpage
--   stolen from Uwe Schmidt, http:\/\/www.haskell.org\/haskellwiki\/HXT
--   and then stolen back again from Holumbus.Utility

computeDocBase  		:: ArrowXml a => a XmlTree String
computeDocBase			= ( ( ( this				-- try to find a base element in head
					/> hasName "html"		-- and compute document base with transfer uri and base
					/> hasName "head"
					/> hasName "base"
					>>> getAttrValue "href"
				      )
				      &&&
				      getAttrValue "transfer-URI"
				    )
				    >>> expandURI
				  )
				  `orElse`
				  getAttrValue "transfer-URI"  		-- the default: take the transfer uri
      

-- ------------------------------------------------------------
--
-- a test application

type TextDoc		= String

type TextDocs		= M.Map URI TextDoc

type HtmlCrawlerConfig	= CrawlerConfig TextDoc TextDocs

emptyTextDocs		:: TextDocs
emptyTextDocs		= M.empty

insertTextDoc		:: (URI, TextDoc) -> TextDocs -> TextDocs
insertTextDoc		= uncurry M.insert

textHtmlCrawlerConfig	:: HtmlCrawlerConfig
textHtmlCrawlerConfig	= ( defaultHtmlCrawlerConfig
			    .
			    defaultCrawlerConfig				-- merge default config with default HTML config
			  $ insertTextDoc					
			  )
			  { cc_readAttributes	= [ (a_validate,   		 v_0)
						  , (a_parse_html,		 v_1)
						  , (a_encoding,		 isoLatin1)
						  , (a_issue_warnings, 		 v_0)
						  , (a_ignore_none_xml_contents, v_1)
						  ]
			  , cc_processDoc	= fromLA (rnfA extractText)	-- force complete evaluation of the result
			  , cc_followRef	= const True			-- the whole world is checked
			  }
    where
    extractText		= xshow ( ( theTitle <+> theBody )
				  >>>
				  deep isText
				)
			  >>^ (words >>> unwords)

    theBody		= this /> hasName "html" /> hasName "body"
    theTitle		= this /> hasName "html" /> hasName "head" /> hasName "title"

textHtmlCrawlerInitState	:: CrawlerState TextDocs
textHtmlCrawlerInitState	= initCrawlerState emptyTextDocs

-- ------------------------------------------------------------

testCrawlerConfig 	:: CrawlerConfig TextDoc TextDocs
testCrawlerConfig	= textHtmlCrawlerConfig
			  { cc_followRef	= simpleFollowRef'
			                          ["http://localhost/~si/klausuren/.*"]
			                          ["()"]
			  }

t1 	:: IO TextDocs
t1	= execCrawler (crawlDoc "http://localhost/~si/") testCrawlerConfig textHtmlCrawlerInitState

t2 	:: IO ((), CrawlerState TextDocs)
t2	= runCrawler  (crawlDoc "http://localhost/~si/") testCrawlerConfig textHtmlCrawlerInitState

-- ------------------------------------------------------------
