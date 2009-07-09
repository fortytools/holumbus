{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.Html
where

import           Data.Function.Selector

import           Data.List
import		 Data.Maybe

import		 Holumbus.Crawler.URIs
import		 Holumbus.Crawler.Core

import		 Text.XML.HXT.Arrow		hiding ( when
						       , getState
						       )
import qualified Text.XML.HXT.Arrow		as X

-- import qualified Debug.Trace			as D

-- ------------------------------------------------------------

defaultHtmlCrawlerConfig	:: AccumulateDocResult a r -> CrawlerConfig a r
defaultHtmlCrawlerConfig op	= ( addReadAttributes [ (a_validate,   		 v_0)
						      , (a_parse_html,		 v_1)
						      , (a_encoding,		 isoLatin1)
						      , (a_issue_warnings, 	 v_0)
						      , (a_ignore_none_xml_contents, v_1)
						      ]
				    >>>
				    setS thePreRefsFilter this
				    >>>
				    setS theProcessRefs getHtmlReferences
				    $ 
				    defaultCrawlerConfig op
				  )

-- ------------------------------------------------------------

-- | Collect all HTML references to other documents within a, frame and iframe elements

getHtmlReferences 		:: ArrowXml a => a XmlTree URI
getHtmlReferences		= fromLA (getRefs $< computeDocBase)
    where
    getRefs base		= deep (hasNameWith ((`elem` ["a","frame","iframe"]) . localPart))
				  >>>
				  ( getAttrValue0 "href"
				    <+>
				    getAttrValue0 "src"
				  )
                                  >>^ toAbsRef base

getDocReferences		:: ArrowXml a => a XmlTree URI
getDocReferences		= fromLA (getRefs $< computeDocBase)
    where
    getRefs base		= multi selRefs >>^ toAbsRef base
				  where
				  hasLocName n	= hasNameWith ((== n) . localPart)
				  selRef en an	= hasLocName en :-> getAttrValue0 an
				  selRefs	= choiceA $
						  map (uncurry selRef) names
						  ++
						  [ appletRefs
						  , objectRefs
						  , this :-> none
						  ]
				  names 	= [ ("img",	"src")
						  , ("input",	"src")		-- input type="image" scr="..."
						  , ("link",	"href")
						  , ("script",	"src")
						  ]				
				  appletRefs	= hasLocName "applet"	:-> (getAppRef $< getAppBase)
						  where
						  getAppBase	= (getAttrValue0 "codebase" `withDefault` ".") >>^ toAbsRef base
						  getAppRef ab	= getAttrValue0 "code" >>^ toAbsRef ab
				  objectRefs	= hasLocName "object"	:-> none	-- TODO

-- | construct an absolute URI by a base URI and a possibly relative URI

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

-- ------------------------------------------------------------

-- | Compute the base URI of a HTML page with respect to a possibly given base element in the head element of a html page.
--   Stolen from Uwe Schmidt, http:\/\/www.haskell.org\/haskellwiki\/HXT
--   and then stolen back again by Uwe from Holumbus.Utility

computeDocBase  		:: ArrowXml a => a XmlTree String
computeDocBase			= ( ( ( getByPath ["html", "head", "base"]
					>>>
					getAttrValue "href"			-- and compute document base with transfer uri and base
				      )
				      &&&
				      getAttrValue transferURI
				    )
				    >>> expandURI
				  )
				  `orElse`
				  getAttrValue transferURI 			-- the default: take the transfer uri

-- ------------------------------------------------------------

getByPath			:: ArrowXml a => [String] -> a XmlTree XmlTree
getByPath			= seqA . map (\ n -> getChildren >>> hasName n)

getHtmlTitle			:: ArrowXml a => a XmlTree String
getHtmlTitle			= getNormalizedText $
				  getByPath ["html", "head", "title"]

getHtmlPlainText		:: ArrowXml a => a XmlTree String
getHtmlPlainText		= getNormalizedText $
				  getByPath ["html", "body"]

getNormalizedText		:: ArrowXml a => a XmlTree XmlTree -> a XmlTree String
getNormalizedText getText'	= ( getText'
				    >>>
				    ( fromLA $ deep getText )
				    >>^
				    (" " ++)						-- text parts are separated by a space
				  )
				  >. (concat >>> normalizeWS)				-- normalize Space

-- ------------------------------------------------------------

-- | normalize whitespace by splitting a text into words and joining this together with unwords

normalizeWS	:: String -> String
normalizeWS	= words >>> unwords

-- | take the first n chars of a string, if the input is too long the cut off is indicated by \"...\" at the end
limitLength	:: Int -> String -> String
limitLength n s
    | length s' <= n		= s
    | otherwise			= take (n - 3) s' ++ "..."
    where
    s'				= take (n + 1) s

-- ------------------------------------------------------------
